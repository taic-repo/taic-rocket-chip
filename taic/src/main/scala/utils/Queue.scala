package taic.utils

import chisel3._
import chisel3.util._

object GQError {
  def queue_full: Int = 1
  def not_init: Int = 2
  def no_send_perm: Int = 4
  def no_recv_perm: Int = 8
  def no_lq: Int = 16
  
}

// 这个模块定义了一套任务队列，包括了这个队列的索引结构和存储结构
// dataWidth：任务标识的位宽
// lq_num: 局部队列的数量
// gq_cap: 全局队列的容量
// 硬件接口定义：
//      alloc_lq：分配局部队列
//      free_lq: 释放局部队列
//      os_proc_in：用于初始化全局队列
//      os_proc_out：全局队列的信息
//      lq_count：已经分配的局部队列的数量
class GlobalQueue(val dataWidth: Int, val lq_num: Int, val gq_cap: Int, val extintr_num: Int, val softintr_num: Int) extends Module {
    val io = IO(new Bundle {
        val os_proc_in = Flipped(Decoupled(UInt((dataWidth * 2).W)))
        val os_proc_out = Output(UInt((dataWidth * 2).W))
        val alloc_lq = Decoupled(UInt(dataWidth.W))
        val free_lq = Flipped(Decoupled(UInt(dataWidth.W)))
        val lq_count = Output(UInt(((log2Ceil(lq_num) + 1).W)))
        val full = Output(Bool())
        val enqs = Vec(lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val deqs = Vec(lq_num, Decoupled(UInt(dataWidth.W)))
        val error = Decoupled(UInt(dataWidth.W))
        val clean = Flipped(Decoupled(Bool()))
        val ext_intrs = Vec(extintr_num, Input(Bool()))
        val register_ext_intr = Vec(extintr_num * lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val register_sender = Vec(lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val cancel_sender = Vec(lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val register_receiver = Vec(lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        // 发送中断的 MMIO 接口
        val send_intr = Vec(lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        // 暴露给控制器的接口
        val send_intr_out = Decoupled(UInt((dataWidth * 4).W))
        // 暴露给控制器的接收中断的接口
        val recv_intr = Flipped(Decoupled(UInt((dataWidth * 2).W)))
        val whartid = Vec(lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val hartid = Output(UInt(dataWidth.W))
        val ssip = Output(Bool())
        val usip = Output(Bool())
    })
    private val ssip = RegInit(false.B)
    private val usip = RegInit(false.B)
    // 记录 hartid，用于向指定的核发送中断
    private val hartid = RegInit(Fill(dataWidth, 1.U))
    private val hartid_arb = Module(new Arbiter(UInt(dataWidth.W), lq_num))
    for (i <- 0 until lq_num) {
        hartid_arb.io.in(i).valid := io.whartid(i).valid
        hartid_arb.io.in(i).bits := io.whartid(i).bits
        io.whartid(i).ready := true.B
    }
    hartid_arb.io.out.ready := true.B
    when(hartid_arb.io.out.fire) {
        hartid := hartid_arb.io.out.bits
    }
    io.hartid := hartid
    io.ssip := ssip
    io.usip := usip

    private val lq_allocator = Module(new BitAllocator(lq_num))
    io.lq_count := lq_allocator.io.alloc_count
    private val os = RegInit(0.U(dataWidth.W))
    private val proc = RegInit(0.U(dataWidth.W))
    io.full := lq_allocator.io.full

    io.os_proc_out := Cat(os, proc)

    private val (
        s_idle :: s_init :: s_lq_init :: s_alloc_lq :: s_free_lq ::
        s_enq0 :: s_enq1 :: s_enq_done :: s_deq0 :: s_deq1 ::
        s_deq_done :: s_clean :: s_extintr0 :: s_extintr1 ::
        s_recv_intr0 :: s_recv_intr1 ::
        s_error :: Nil
    ) = Enum(17)


    private val state = RegInit(s_idle)

    private val lqmetas = Seq.fill(lq_num)(Module(new LqMeta(dataWidth, gq_cap)))
    // 正在运行的任务标识
    private val currs = RegInit(VecInit(Seq.fill(lq_num)(0.U(dataWidth.W))))
    private val lq_inited = Cat(Seq.tabulate(lq_num) { i => lqmetas(i).io.set_head.fire }.reverse)

    private val data_array = Module(new DataArray(dataWidth, gq_cap))
    private val data_array_idx = RegInit(0.U(dataWidth.W))
    private val enq_data = RegInit(0.U(dataWidth.W))
    data_array.io.idx := data_array_idx
    data_array.io.enq.bits := enq_data
    data_array.io.enq.valid := state === s_enq1
    data_array.io.deq.ready := state === s_deq1
    private val task_count = data_array.io.count
    private val error_code = RegInit(0.U(dataWidth.W))
    io.error.bits := error_code
    io.error.valid := true.B


    // 当 valid 信号为 1 时，表示有任务入队
    private val has_enq = Cat(Seq.tabulate(lq_num) { i => io.enqs(i).valid }.reverse)
    private val enq_idx = RegInit(0.U(dataWidth.W))

    // 当 ready 信号为 1 时，表示有任务出队
    private val has_deq = Cat(Seq.tabulate(lq_num) { i => io.deqs(i).ready }.reverse)
    private val deq_idx = RegInit(0.U(dataWidth.W))
    private val req_deq_idx = RegInit(0.U(dataWidth.W))
    private val deq_dones = Cat(Seq.tabulate(lq_num) { i => io.deqs(i).fire }.reverse)

    lqmetas(0).io.set_head.bits := 0.U
    for (i <- 1 until lq_num) {
        lqmetas(i).io.set_head.bits := Mux(state === s_lq_init, lqmetas(i - 1).io.tail, Mux(state === s_enq0, lqmetas(i).io.head + 1.U, Mux(state === s_deq0, lqmetas(i).io.head - 1.U, lqmetas(i).io.head)))
    }
    // 只有在完成之后，才会输出握手信号
    for (i <- 0 until lq_num) {
        lqmetas(i).io.deq.bits := false.B
        lqmetas(i).io.enq.bits := false.B
        io.enqs(i).ready := (state === s_enq_done && enq_idx === i.U) || state === s_error
        io.deqs(i).valid := (state === s_deq_done && req_deq_idx === i.U) || state === s_error
        io.deqs(i).bits := Mux(state === s_error, 0.U, data_array.io.deq.bits)
        lqmetas(i).io.enq.valid := state === s_enq0 && enq_idx === i.U
        lqmetas(i).io.deq.valid := state === s_deq0 && deq_idx === i.U
        lqmetas(i).io.set_head.valid := Mux(state === s_lq_init && lq_allocator.io.alloc.bits === i.U, true.B, (state === s_enq0 && enq_idx < i.U) || (state === s_deq0 && deq_idx < i.U))
    }

    io.os_proc_in.ready := state === s_idle

    io.alloc_lq.valid := state === s_alloc_lq
    io.alloc_lq.bits <> lq_allocator.io.alloc.bits
    lq_allocator.io.alloc.ready := io.alloc_lq.fire

    io.free_lq.ready := state === s_idle
    lq_allocator.io.free.bits := io.free_lq.bits
    lq_allocator.io.free.valid := io.free_lq.fire

    private val lq_not_emptys = Cat(Seq.tabulate(lq_num) { i => ~lqmetas(i).io.empty }.reverse)
    
    when(state === s_idle && has_deq =/= 0.U) {
        req_deq_idx := PriorityEncoder(has_deq)
        // 如果存在中断，则从第一个局部队列中取出任务，否则从非空的局部队列中取出任务
        deq_idx := Mux(ssip | usip, 0.U, PriorityEncoder(Mux((has_deq & lq_not_emptys) =/= 0.U, has_deq, lq_not_emptys)))
        // 无论是否存在中断，在下一次取出任务时，都将中断清空
        ssip := false.B
        usip := false.B
        when(os === 0.U && proc === 0.U) {
            error_code := error_code | GQError.not_init.U     // 队列没有初始化，即错误使用资源
            state := s_error
        }.elsewhen((has_deq & lq_allocator.io.alloced) === 0.U ) {
            error_code := error_code | GQError.no_lq.U     // 对应的局部队列没有分配
            state := s_error
        }.otherwise {
            state := s_deq0
        }
    }

    when(state === s_deq0) {
        for(i <- 0 until lq_num) {
            when(i.U === deq_idx) {
                data_array_idx := lqmetas(i).io.head
            }
        }
        state := s_deq1
    }

    when(state === s_deq1 && data_array.io.deq.fire) {
        for(i <- 0 until lq_num) {
            when(i.U === req_deq_idx) {
                currs(i) := data_array.io.deq.bits
            }
        }
        state := s_deq_done
    }

    when(state === s_deq_done && deq_dones =/= 0.U) {
        state := s_idle
    }

    private val is_waked = RegInit(false.B)
    when(state === s_idle && has_enq =/= 0.U) {
        enq_idx := PriorityEncoder(has_enq)
        is_waked := false.B
        when(task_count === gq_cap.U) {
            error_code := error_code | GQError.queue_full.U       // 队列已满错误
            state := s_error
        }.elsewhen(os === 0.U && proc === 0.U) {
            error_code := error_code | GQError.not_init.U     // 队列没有初始化，即错误使用资源
            state := s_error
        }.otherwise {
            state := s_enq0
        }
    }

    // 当软件读出错误后，就会消除错误
    when(io.error.fire) {
        error_code := 0.U
        state := s_error
    }

    // 出现了 error，不应该影响其他的功能，只需要提供相应的错误码即可，标识错误的原因，让软件能够获取到
    when(state === s_error) {
        state := s_idle
    }
    when(state === s_enq0) {
        for(i <- 0 until lq_num) {
            when(i.U === enq_idx) {
                enq_data := Mux(is_waked, enq_data, io.enqs(i).bits)
                // 根据是否需要抢占，来判断将其放入队头还是队尾
                data_array_idx := Mux(is_waked & (enq_data(0) === 1.U), lqmetas(i).io.head, lqmetas(i).io.tail)
                // 如果是内核，则 proc === 0，需要根据抢占标记拉高 ssip
                ssip := Mux(proc === 0.U, is_waked & (enq_data(0) === 1.U), 0.U)
                // 如果是用户，则 proc =/= 1，需要根据抢占标记拉高 usip
                usip := Mux(proc =/= 0.U, is_waked & (enq_data(0) === 1.U), 0.U)
            }
        }
        state := s_enq1
    }

    when(state === s_enq1 && data_array.io.enq.fire) {
        state := s_enq_done
    }

    when(state === s_enq_done) {
        state := s_idle
    }
    
    when(state === s_idle && io.os_proc_in.fire) {
        os := io.os_proc_in.bits(dataWidth * 2 - 1, dataWidth)
        proc := io.os_proc_in.bits(dataWidth - 1, 0)
        state := s_init
    }
    when(state === s_init) {
        state := s_idle
    }
    when(state === s_idle && io.alloc_lq.ready) {
        state := s_lq_init
    }
    when(state === s_lq_init && lq_inited =/= 0.U) {
        state := s_alloc_lq
    }
    when(state === s_alloc_lq && io.alloc_lq.fire) {
        state := s_idle
    }
    when(state === s_idle && io.free_lq.fire) {
        state := s_free_lq
    }
    when(state === s_free_lq) {
        state := s_idle
    }
    io.clean.ready := (state === s_idle) || (state === s_free_lq)
    when(((state === s_idle) || (state === s_free_lq)) && io.clean.fire) {
        os := 0.U
        proc := 0.U
        Seq.tabulate(lq_num) { i =>
            currs(i) := 0.U
        }
        error_code := 0.U
        // 清除 lqmetas 中的 count，将所有的局部队列清空
        state := s_clean
    }
    for (i <- 0 until lq_num) {
        lqmetas(i).io.clean.valid := ((state === s_idle) || (state === s_free_lq)) && io.clean.fire
        lqmetas(i).io.clean.bits := false.B
    }
    data_array.io.clean.valid := ((state === s_idle) || (state === s_free_lq)) && io.clean.fire
    data_array.io.clean.bits := false.B
    when(state === s_clean) {
        state := s_idle
    }

    // 外部中断相关的逻辑
    private val sext_idle :: sext_reg_extintr :: Nil = Enum(2)
    private val sext_state = RegInit(sext_idle)

    private val ext_intr_slots = Module(new ExtIntrSlots(extintr_num, dataWidth))

    ext_intr_slots.io.clean.valid := ((state === s_idle) || (state === s_free_lq)) && io.clean.fire
    ext_intr_slots.io.clean.bits := false.B

    private val extintr_arbs = Seq.fill(extintr_num)(Module(new Arbiter(UInt(dataWidth.W), lq_num)))
    for (i <- 0 until extintr_num) {
        for (j <- 0 until lq_num) {
            extintr_arbs(i).io.in(j).valid := io.register_ext_intr(j * extintr_num + i).valid
            extintr_arbs(i).io.in(j).bits := io.register_ext_intr(j * extintr_num + i).bits
        }
        extintr_arbs(i).io.out <> ext_intr_slots.io.register(i)
    }
    Seq.tabulate(lq_num * extintr_num) { i =>
        io.register_ext_intr(i).ready := (sext_state === sext_idle) && (state =/= s_clean)
    }
    private val has_register_extintr = Cat(Seq.tabulate(extintr_num * lq_num) { i => io.register_ext_intr(i).valid }.reverse)
    // private val regester_extintr_done = Cat(Seq.tabulate(extintr_num) { i => ext_intr_slots.io.register(i).fire }.reverse)
    when((sext_state === sext_idle) && (state =/= s_clean) && has_register_extintr =/= 0.U) {
        sext_state := sext_reg_extintr
    }
    when(sext_state === sext_reg_extintr) {
        sext_state := sext_idle
    }

    // extintr_bits 用于记录需要处理的中断，只会在第一次产生中断时触发，变为 true
    private val extintr_bits = RegInit(VecInit(Seq.fill(extintr_num + 1)(false.B)))
    private val ext_intr_idx = PriorityEncoder(extintr_bits)
    // 在中断信号的上升沿更新
    for (i <- 0 until extintr_num) {
        when(io.ext_intrs(i) && !RegNext(io.ext_intrs(i))) {
            extintr_bits(i) := true.B
        }
    }

    // 外部中断处理逻辑，找到对应的中断源以及处理任务
    when(state === s_idle && ext_intr_idx =/= extintr_num.U) {
        state := s_extintr0
    }
    for (i <- 0 until extintr_num) {
        ext_intr_slots.io.wake_handler(i).ready := state === s_extintr0 && i.U === ext_intr_idx
    }
    when(state === s_extintr0) {
        // 始终将外部中断的处理任务放在第一个局部队列中
        enq_idx := 0.U
        is_waked := true.B
        for (i <- 0 until extintr_num) {
            when(i.U === ext_intr_idx) {
                enq_data := ext_intr_slots.io.wake_handler(i).bits
                // 清除对应的位
                extintr_bits(i) := false.B
            }
        }
        state := s_extintr1
    }
    when(state === s_extintr1) {
        when(enq_data =/= 0.U) {
            state := s_enq0
        }.otherwise {
            state := s_idle
        }
    }

    // 软件中断相关逻辑
    private val (
        sint_idle :: sint_reg_send0 :: sint_reg_send1 :: 
        sint_reg_recv0 :: sint_reg_recv1 :: sint_reg_recv2 :: 
        sint_cancel_send0 :: sint_cancel_send1 ::
        sint_sendintr0 :: sint_sendintr1 :: Nil
    ) = Enum(10)
    private val softintr_state = RegInit(sint_idle)

    private val softintr_slots = Module(new SoftIntrSlots(softintr_num, dataWidth))
    softintr_slots.io.clean.valid := ((state === s_idle) || (state === s_free_lq)) && io.clean.fire
    softintr_slots.io.clean.bits := false.B
    private val register_sender_arb = Module(new Arbiter(UInt(dataWidth.W), lq_num))
    private val cancel_sender_arb = Module(new Arbiter(UInt(dataWidth.W), lq_num))
    private val register_receiver_arb = Module(new Arbiter(UInt(dataWidth.W), lq_num))
    for (i <- 0 until lq_num) {
        register_sender_arb.io.in(i).valid := io.register_sender(i).valid
        register_sender_arb.io.in(i).bits := io.register_sender(i).bits
        cancel_sender_arb.io.in(i).valid := io.cancel_sender(i).valid
        cancel_sender_arb.io.in(i).bits := io.cancel_sender(i).bits
        register_receiver_arb.io.in(i).valid := io.register_receiver(i).valid
        register_receiver_arb.io.in(i).bits := io.register_receiver(i).bits

        io.register_sender(i).ready := softintr_state === sint_idle || softintr_state === sint_reg_send0
        io.cancel_sender(i).ready := softintr_state === sint_idle || softintr_state === sint_cancel_send0
        io.register_receiver(i).ready := softintr_state === sint_idle || softintr_state === sint_reg_recv0 || softintr_state === sint_reg_recv1
    }
    register_sender_arb.io.out <> softintr_slots.io.register_sender
    cancel_sender_arb.io.out <> softintr_slots.io.cancel_sender
    register_receiver_arb.io.out <> softintr_slots.io.register_receiver
    // 注册相关逻辑
    private val has_register_sender = Cat(Seq.tabulate(lq_num) { i => io.register_sender(i).valid }.reverse)
    private val has_cancel_sender = Cat(Seq.tabulate(lq_num) { i => io.cancel_sender(i).valid }.reverse)
    private val has_register_receiver = Cat(Seq.tabulate(lq_num) { i => io.register_receiver(i).valid }.reverse)
    when(softintr_state === sint_idle && has_register_sender =/= 0.U && register_sender_arb.io.out.fire) {
        softintr_state := sint_reg_send0
    }
    when(softintr_state === sint_reg_send0 && has_register_sender =/= 0.U && register_sender_arb.io.out.fire) {
        softintr_state := sint_reg_send1
    }
    when(softintr_state === sint_reg_send1) {
        softintr_state := sint_idle
    }

    when(softintr_state === sint_idle && has_cancel_sender =/= 0.U && cancel_sender_arb.io.out.fire) {
        softintr_state := sint_cancel_send0
    }
    when(softintr_state === sint_cancel_send0 && has_cancel_sender =/= 0.U && cancel_sender_arb.io.out.fire) {
        softintr_state := sint_cancel_send1
    }
    when(softintr_state === sint_cancel_send1) {
        softintr_state := sint_idle
    }

    when(softintr_state === sint_idle && has_register_receiver =/= 0.U && register_receiver_arb.io.out.fire) {
        softintr_state := sint_reg_recv0
    }
    when(softintr_state === sint_reg_recv0 && has_register_receiver =/= 0.U && register_receiver_arb.io.out.fire) {
        softintr_state := sint_reg_recv1
    }
    when(softintr_state === sint_reg_recv1 && has_register_receiver =/= 0.U && register_receiver_arb.io.out.fire) {
        softintr_state := sint_reg_recv2
    }
    when(softintr_state === sint_reg_recv2) {
        softintr_state := sint_idle
    }

    // 发送中断的逻辑
    private val send_intr_arb = Module(new Arbiter(UInt(dataWidth.W), lq_num))
    for (i <- 0 until lq_num) {
        send_intr_arb.io.in(i).valid := io.send_intr(i).valid
        send_intr_arb.io.in(i).bits := io.send_intr(i).bits
        io.send_intr(i).ready := softintr_state === sint_idle || softintr_state === sint_sendintr0
        dontTouch(io.send_intr(i))
    }
    private val send_intr_bits = dontTouch(RegNext(send_intr_arb.io.out.bits))
    send_intr_arb.io.out.ready := softintr_state === sint_idle
    private val send_intr_arb_bits = dontTouch(send_intr_arb.io.out.bits)
    private val recv_os = dontTouch(RegInit(0.U(dataWidth.W)))
    private val recv_proc = dontTouch(RegInit(0.U(dataWidth.W)))
    private val has_send_intr = Cat(Seq.tabulate(lq_num) { i => io.send_intr(i).fire }.reverse)
    when(softintr_state === sint_idle && has_send_intr =/= 0.U) {
        recv_os := send_intr_arb_bits
        softintr_state := sint_sendintr0
    }
    when(softintr_state === sint_sendintr0 && has_send_intr =/= 0.U) {
        recv_proc := send_intr_arb_bits
        softintr_state := sint_sendintr1
    }
    private val recv_os_proc_matches = Cat(Seq.tabulate(softintr_num) { i => softintr_slots.io.can_send(i) === Cat(recv_os, recv_proc) }.reverse)
    io.send_intr_out.bits := Cat(recv_os, recv_proc, os, proc)
    private val send_ok = RegInit(false.B)
    io.send_intr_out.valid := send_ok
    // 在匹配成功后，拉起 send_ok，表示可以发送中断；当控制器接收到信号后，清除 send_ok
    send_ok := Mux(softintr_state === sint_sendintr1 && recv_os_proc_matches =/= 0.U, true.B, Mux(io.send_intr_out.fire, false.B, send_ok))
    when(softintr_state === sint_sendintr1) {
        when(recv_os_proc_matches === 0.U) {
            error_code := error_code | GQError.no_send_perm.U
        }
        softintr_state := sint_idle
    }

    // 接收软件中断的逻辑
    // 只能在处于 idle 的情况下接收软件中断
    io.recv_intr.ready := state === s_idle
    
    private val send_os_proc = RegInit(0.U((dataWidth * 2).W))
    when(state === s_idle && io.recv_intr.fire) {
        send_os_proc := io.recv_intr.bits
        state := s_recv_intr0
    }
    private val send_os_proc_matches = Cat(Seq.tabulate(softintr_num) { i => softintr_slots.io.can_recv(i) === send_os_proc }.reverse)
    private val handler_idx = PriorityEncoder(Cat(0.U, send_os_proc_matches))
    for(i <- 0 until softintr_num) {
        softintr_slots.io.wake_handler(i).ready := state === s_recv_intr0 && i.U === handler_idx
    }
    when(state === s_recv_intr0) {
        // 始终将软件中断的处理任务放在第一个局部队列中
        enq_idx := 0.U
        is_waked := true.B
        for (i <- 0 until softintr_num) {
            when(i.U === handler_idx) {
                enq_data := softintr_slots.io.wake_handler(i).bits
            }
        }
        state := s_recv_intr1     
    }
    when(state === s_recv_intr1) {
        when(enq_data =/= 0.U) {
            send_os_proc := 0.U
            state := s_enq0
        }.otherwise {
            send_os_proc := 0.U
            error_code := error_code | GQError.no_recv_perm.U
            state := s_error
        }
    }
}

// LqMeta：表示局部队列的元数据，包括以下信息：
//      head：队头指针，实际上是队头任务在全局队列中的下标
//      count：局部队列中的任务数量
//
// 硬件接口定义：
//      set_head：设置局部队列头指针
//      head：获取局部队列的队头
//      tail：获取局部队列的队尾
//      enq：任务入队
//      deq：任务出队
// dataWidth：任务标识的位宽
// gq_cap：全局队列的容量
class LqMeta(val dataWidth: Int, val gq_cap: Int) extends Module {
    val io = IO(new Bundle {
        val set_head = Flipped(Decoupled(UInt(log2Ceil(gq_cap).W)))
        val head = Output(UInt(log2Ceil(gq_cap).W))
        val tail = Output(UInt(log2Ceil(gq_cap).W))
        val enq = Flipped(Decoupled(Bool()))
        val deq = Flipped(Decoupled(Bool()))
        val empty = Output(Bool())
        val clean = Flipped(Decoupled(Bool()))
    })
    private val head = RegInit(0.U(log2Ceil(gq_cap).W))
    private val count = RegInit(0.U(log2Ceil(gq_cap).W))
    io.empty := count === 0.U

    io.head := head
    io.tail := (head + count) % gq_cap.U

    private val s_idle :: s_set_head :: s_enq :: s_deq :: Nil = Enum(4)
    private val state = RegInit(s_idle)

    io.set_head.ready := state === s_idle

    when(state === s_idle && io.set_head.fire) {
        head := io.set_head.bits
        state := s_set_head
    }

    when(state === s_set_head) {
        state := s_idle
    }

    io.enq.ready := state === s_idle
    io.deq.ready := state === s_idle
    io.clean.ready := state === s_idle
    when(state === s_idle && io.clean.fire) {
        count := 0.U
        head := 0.U
    }

    when(state === s_idle && io.enq.fire) {
        count := Mux(count === gq_cap.U, count, count + 1.U)
        state := s_enq
    }
    when(state === s_enq) {
        state := s_idle
    }
    when(state === s_idle && io.deq.fire) {
        count := Mux(count === 0.U, 0.U, count - 1.U)
        state := s_deq
    }
    when(state === s_deq) {
        state := s_idle
    }
}
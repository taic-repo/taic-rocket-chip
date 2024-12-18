package taic.utils

import chisel3._
import chisel3.util._

// Controller: 控制器，用于分配队列资源，以及队列操作
// 
class Controller(val gq_num: Int, val lq_num: Int, val gq_cap: Int, val dataWidth: Int, val extintr_num: Int, val softintr_num: Int) extends Module {
    val io = IO(new Bundle {
        /************** 全局的接口 ******************/
        // 用于分配一个局部队列，写接口
        val alloc = Flipped(Decoupled(UInt(dataWidth.W)))
        // 在分配完成后，从这个接口读出对应的局部队列编号
        val alloced = Decoupled(UInt(dataWidth.W))
        // 用于释放一个局部队列，写接口
        val free = Flipped(Decoupled(UInt(dataWidth.W)))
        /*******************************************/

        /************** 局部的接口 ******************/
        val enqs = Vec(gq_num * lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val deqs = Vec(gq_num * lq_num, Decoupled(UInt(dataWidth.W)))
        val errors = Vec(gq_num, Decoupled(UInt(dataWidth.W)))
        val ext_intrs = Vec(extintr_num, Input(Bool()))
        val register_ext_intr = Vec(extintr_num * gq_num * lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        /*******************************************/

        /************** 与软件中断相关的接口 ******************/
        val register_sender = Vec(gq_num * lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val cancel_sender = Vec(gq_num * lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val register_receiver = Vec(gq_num * lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        // MMIO 接口
        val send_intr = Vec(gq_num * lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        // hartid
        val whartid = Vec(gq_num * lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val hartids = Vec(gq_num, Output(UInt(dataWidth.W)))
        val ssips = Vec(gq_num, Output(Bool()))
        val usips = Vec(gq_num, Output(Bool()))
    })

    // 申请时需要记录的信息
    private val os = RegInit(0.U(dataWidth.W))
    private val proc = RegInit(0.U(dataWidth.W))

    private val gq_allocator = Module(new BitAllocator(gq_num))
    private val alloced_gq = RegInit(0.U(log2Ceil(gq_num).W))
    // 全局队列
    private val gqs = Seq.fill(gq_num)(Module(new GlobalQueue(dataWidth, lq_num, gq_cap, extintr_num, softintr_num)))

    private val (
        s_idle :: s_wos :: s_wproc :: s_find :: s_alloc_gq :: 
        s_gq_init :: s_alloc_lq :: s_ridx :: s_free_lq :: 
        s_free_gq :: s_error :: 
        s_pass_sint0 :: s_pass_sint1 :: Nil 
    ) = Enum(13)

    private val state = RegInit(s_idle)

    io.alloc.ready := (state === s_idle) || (state === s_wos)
    gq_allocator.io.alloc.ready := state === s_alloc_gq
    

    when(state === s_idle && io.alloc.fire) {
        os := io.alloc.bits
        state := s_wos
    }
    when(state === s_wos && io.alloc.fire) {
        proc := io.alloc.bits
        state := s_wproc
    }

    // 查找组合逻辑，对于容量较小的情况，可以快速查找完成
    private val os_proc_matches = Cat(Seq.tabulate(gq_num) { i => gqs(i).io.os_proc_out === Cat(os, proc) }.reverse)
    private val os_proc_idx = PriorityEncoder(Cat(0.U, os_proc_matches))
    // 判断是否完成初始化
    for (i <- 0 until gq_num) {
        gqs(i).io.os_proc_in.valid := state === s_gq_init && alloced_gq === i.U
        gqs(i).io.os_proc_in.bits := Cat(os, proc)
        Seq.tabulate(lq_num) { j => 
            io.enqs(i * lq_num + j) <> gqs(i).io.enqs(j)
            io.deqs(i * lq_num + j) <> gqs(i).io.deqs(j)
            io.register_sender(i * lq_num + j) <> gqs(i).io.register_sender(j)
            io.cancel_sender(i * lq_num + j) <> gqs(i).io.cancel_sender(j)
            io.register_receiver(i * lq_num + j) <> gqs(i).io.register_receiver(j)
            io.send_intr(i * lq_num + j) <> gqs(i).io.send_intr(j)
            dontTouch(io.send_intr(i * lq_num + j))
            io.whartid(i * lq_num + j) <> gqs(i).io.whartid(j)
            io.hartids(i) := gqs(i).io.hartid
            io.ssips(i) := gqs(i).io.ssip
            io.usips(i) := gqs(i).io.usip
        }
        io.errors(i) <> gqs(i).io.error
        Seq.tabulate(extintr_num) { j =>
            gqs(i).io.ext_intrs(j) := io.ext_intrs(j)
        }
        Seq.tabulate(extintr_num * lq_num) { j =>
            io.register_ext_intr(i * extintr_num * lq_num + j) <> gqs(i).io.register_ext_intr(j)
        }
    }
    private val gq_inited = Cat(Seq.tabulate(gq_num) { i => gqs(i).io.os_proc_in.fire }.reverse)
    // 判断是否完成局部队列分配
    for (i <- 0 until gq_num) {
        gqs(i).io.alloc_lq.ready := state === s_alloc_lq && os_proc_idx === i.U
    }
    private val lq_allocated = Cat(Seq.tabulate(gq_num) { i => gqs(i).io.alloc_lq.fire }.reverse)


    when(state === s_wproc) {
        state := s_find
    }

    when(state === s_find && os_proc_idx === gq_num.U) { // 没有找到匹配的 os_proc，需要先分配全局队列，再分配局部队列
        when(gq_allocator.io.full) {
            state := s_error
        }.otherwise {
            state := s_alloc_gq
        }
    }
    when(state === s_find && os_proc_idx =/= gq_num.U) { // 找到匹配的 os_proc，直接分配局部队列
        // 判断局部队列是否已经满了
        for (i <- 0 until gq_num) {
            when(os_proc_idx === i.U) {
                when(gqs(i).io.full) {
                    state := s_error
                }.otherwise {
                    state := s_alloc_lq
                }
            }
        }
    }

    when(state === s_alloc_gq && gq_allocator.io.alloc.fire) {    // 先分配全局队列
        alloced_gq := gq_allocator.io.alloc.bits
        state := s_gq_init
    }

    when(state === s_gq_init && gq_inited =/= 0.U) { // 全局队列初始化完成
        state := s_alloc_lq
    }

    private val lq_idx = RegInit(0.U((dataWidth / 2).W))
    when(state === s_alloc_lq && lq_allocated =/= 0.U) { // 局部队列分配完成
        for (i <- 0 until gq_num) {
            when(os_proc_idx === i.U) {
                lq_idx := gqs(i).io.alloc_lq.bits
            }
        }
        state := s_ridx
    }

    io.alloced.valid := state === s_ridx || state === s_error
    io.alloced.bits := Mux(state === s_ridx, Cat(os_proc_idx, lq_idx), Fill(dataWidth, 1.U))
    when(state === s_ridx && io.alloced.fire) {
        state := s_idle
    }
    when(state === s_error && io.alloced.fire) {
        state := s_idle
    }

    io.free.ready := state === s_idle
    private val free_idx = RegInit(0.U(dataWidth.W))
    private val free_gq_idx = free_idx(dataWidth - 1, dataWidth / 2)
    private val free_lq_idx = free_idx(dataWidth / 2 - 1, 0)
    when(state === s_idle && io.free.fire) {
        free_idx := io.free.bits
        state := s_free_lq
    }
    for (i <- 0 until gq_num) {
        gqs(i).io.free_lq.valid := state === s_free_lq && free_gq_idx === i.U
        gqs(i).io.free_lq.bits := free_lq_idx
    }
    private val lq_freed = Cat(Seq.tabulate(gq_num) { i => gqs(i).io.free_lq.fire }.reverse)
    private val gq_empty = Cat(Seq.tabulate(gq_num) { i => gqs(i).io.lq_count === 0.U }.reverse)
    private val last_gq_empty = RegNext(gq_empty)
    gq_allocator.io.free.bits := free_gq_idx
    gq_allocator.io.free.valid := state === s_free_gq && ((last_gq_empty ^ gq_empty) =/= 0.U)
    private val clean_gq_idx = PriorityEncoder(Cat(0.U, (last_gq_empty ^ gq_empty)(gq_num - 1, 0)))
    Seq.tabulate(gq_num) { i =>
        gqs(i).io.clean.valid := state === s_free_gq && clean_gq_idx === i.U
        gqs(i).io.clean.bits := false.B
    }

    when(state === s_free_lq && lq_freed =/= 0.U) {
        state := s_free_gq
    }

    when(state === s_free_gq) {
        state := s_idle
    }

    // 与软件中断相关的逻辑
    private val has_sintr = Cat(Seq.tabulate(gq_num) { i => gqs(i).io.send_intr_out.valid }.reverse)
    private val sintr_idx = PriorityEncoder(Cat(0.U, has_sintr))
    when(state === s_idle && has_sintr =/= 0.U) {
        state := s_pass_sint0
    }
    for (i <- 0 until gq_num) {
        gqs(i).io.send_intr_out.ready := state === s_pass_sint0 && sintr_idx === i.U
    }
    private val send_os = RegInit(0.U(dataWidth.W))
    private val send_proc = RegInit(0.U(dataWidth.W))
    when(state === s_pass_sint0) {
        for(i <- 0 until gq_num) {
            when(sintr_idx === i.U) {
                os := gqs(i).io.send_intr_out.bits(dataWidth * 4 - 1, dataWidth * 3)
                proc := gqs(i).io.send_intr_out.bits(dataWidth * 3 - 1, dataWidth * 2)
                send_os := gqs(i).io.send_intr_out.bits(dataWidth * 2 - 1, dataWidth)
                send_proc := gqs(i).io.send_intr_out.bits(dataWidth - 1, 0)
            }
        }
        state := s_pass_sint1
    }
    for (i <- 0 until gq_num) {
        gqs(i).io.recv_intr.valid := state === s_pass_sint1 && os_proc_idx === i.U
        gqs(i).io.recv_intr.bits := Cat(send_os, send_proc)
    }
    private val has_recved = Cat(Seq.tabulate(gq_num) { i => gqs(i).io.recv_intr.fire }.reverse)
    when(state === s_pass_sint1 && has_recved =/= 0.U) {
        state := s_idle
    }
    // 接收方不在线，或者接收方不存在，这种处理是直接丢弃掉这个中断，后续需要增加额外的处理，来判断接收方是否存在
    when(state === s_pass_sint1 && os_proc_idx === gq_num.U) {
        state := s_idle
    }

}
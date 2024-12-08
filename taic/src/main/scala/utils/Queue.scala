package taic.utils

import chisel3._
import chisel3.util._

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
class GlobalQueue(val dataWidth: Int, val lq_num: Int, val gq_cap: Int) extends Module {
    val io = IO(new Bundle {
        val os_proc_in = Flipped(Decoupled(UInt((dataWidth * 2).W)))
        val os_proc_out = Output(UInt((dataWidth * 2).W))
        val alloc_lq = Decoupled(UInt(dataWidth.W))
        val free_lq = Flipped(Decoupled(UInt(dataWidth.W)))
        val lq_count = Output(UInt(((log2Ceil(lq_num) + 1).W)))
        val full = Output(Bool())
        val enqs = Vec(lq_num, Flipped(Decoupled(UInt(dataWidth.W))))
        val deqs = Vec(lq_num, Decoupled(UInt(dataWidth.W)))
    })

    private val lq_allocator = Module(new BitAllocator(lq_num))
    io.lq_count := lq_allocator.io.alloc_count
    private val os = RegInit(0.U(dataWidth.W))
    private val proc = RegInit(0.U(dataWidth.W))
    io.full := lq_allocator.io.full
    private val task_count = RegInit(0.U((log2Ceil(gq_cap) + 1).W))

    io.os_proc_out := Cat(os, proc)

    private val s_idle :: s_init :: s_lq_init :: s_alloc_lq :: s_free_lq :: s_enq0 :: s_enq1 :: s_enq_done :: s_deq0 :: s_deq1 :: s_deq_done :: Nil = Enum(11)
    private val state = RegInit(s_idle)

    private val lqmetas = Seq.fill(lq_num)(Module(new LqMeta(dataWidth, gq_cap)))
    private val lq_inited = Cat(Seq.tabulate(lq_num) { i => lqmetas(i).io.set_head.fire }.reverse)

    // 当 valid 信号为 1 时，表示有任务入队
    private val has_enq = Cat(Seq.tabulate(lq_num) { i => io.enqs(i).valid }.reverse)
    private val enq_idx = RegInit(0.U(dataWidth.W))
    private val enq_dones = Cat(Seq.tabulate(lq_num) { i => io.enqs(i).fire }.reverse)

    // 当 ready 信号为 1 时，表示有任务出队
    private val has_deq = Cat(Seq.tabulate(lq_num) { i => io.deqs(i).ready }.reverse)
    private val deq_idx = RegInit(0.U(dataWidth.W))
    private val deq_dones = Cat(Seq.tabulate(lq_num) { i => io.deqs(i).fire }.reverse)

    lqmetas(0).io.set_head.bits := 0.U
    for (i <- 1 until lq_num) {
        lqmetas(i).io.set_head.bits := Mux(state === s_lq_init, lqmetas(i - 1).io.tail, Mux(state === s_enq0, lqmetas(i).io.head + 1.U, Mux(state === s_deq0, lqmetas(i).io.head - 1.U, lqmetas(i).io.head)))
    }
    // 只有在完成之后，才会输出握手信号
    for (i <- 0 until lq_num) {
        lqmetas(i).io.deq.bits := false.B
        lqmetas(i).io.enq.bits := false.B
        io.enqs(i).ready := state === s_enq_done && enq_idx === i.U
        io.deqs(i).valid := state === s_deq_done && deq_idx === i.U
        io.deqs(i).bits := lqmetas(i).io.head // TODO：这里需要修改成实际的任务标识，需要在增加了存储空间之后再来进行修改
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
    
    when(state === s_idle && has_deq =/= 0.U) {
        deq_idx := PriorityEncoder(has_deq)
        state := s_deq0
    }

    when(state === s_deq0) {
        state := s_deq1
    }

    when(state === s_deq1) {
        state := s_deq_done
    }

    when(state === s_deq_done && deq_dones =/= 0.U) {
        state := s_idle
    }

    when(state === s_idle && has_enq =/= 0.U) {
        enq_idx := PriorityEncoder(has_enq)
        state := s_enq0
    }

    when(state === s_enq0) {
        state := s_enq1
    }

    when(state === s_enq1) {
        state := s_enq_done
    }

    when(state === s_enq_done && enq_dones =/= 0.U) {
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

}

// LqMeta：表示局部队列的元数据，包括以下信息：
//      curr：正在运行的任务标识
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
    })
    private val curr = RegInit(0.U(dataWidth.W))
    private val head = RegInit(0.U(log2Ceil(gq_cap).W))
    private val count = RegInit(0.U(log2Ceil(gq_cap).W))

    io.head := head
    io.tail := (head + count) % gq_cap.U

    private val s_idle :: s_set_head :: s_set_curr :: s_enq :: s_deq :: Nil = Enum(5)
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
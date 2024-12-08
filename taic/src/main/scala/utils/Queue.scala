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
    })

    private val lq_allocator = Module(new BitAllocator(lq_num))
    io.lq_count := lq_allocator.io.alloc_count
    private val os = RegInit(0.U(dataWidth.W))
    private val proc = RegInit(0.U(dataWidth.W))
    io.full := lq_allocator.io.full

    io.os_proc_out := Cat(os, proc)

    private val s_idle :: s_init :: s_alloc_lq :: s_free_lq :: Nil = Enum(4)
    private val state = RegInit(s_idle)

    io.os_proc_in.ready := state === s_idle

    io.alloc_lq.valid := state === s_idle
    io.alloc_lq.bits <> lq_allocator.io.alloc.bits
    lq_allocator.io.alloc.ready := io.alloc_lq.fire

    io.free_lq.ready := state === s_idle
    lq_allocator.io.free.bits := io.free_lq.bits
    lq_allocator.io.free.valid := io.free_lq.fire
    
    when(state === s_idle && io.os_proc_in.fire) {
        os := io.os_proc_in.bits(dataWidth * 2 - 1, dataWidth)
        proc := io.os_proc_in.bits(dataWidth - 1, 0)
        state := s_init
    }
    when(state === s_init) {
        state := s_idle
    }
    when(state === s_idle && io.alloc_lq.fire) {
        state := s_alloc_lq
    }
    when(state === s_alloc_lq) {
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
//      ptr：队头指针，实际上是队头任务在全局队列中的下标
//      count：局部队列中的任务数量
//
// 硬件接口定义：
//      op：对元数据进行的操作
//          0：设置 curr
//          1：设置 ptr
//          2：任务入队：ptr 不变，count 加一
//          3：任务出队：ptr 加一，count 减一
//          4：ptr 加一
//          5：ptr 减一
//      head：获取局部队列的队头
// dataWidth：任务标识的位宽
// gq_cap：全局队列的容量
class LqMeta(val dataWidth: Int, val gq_cap: Int) extends Module {
    val io = IO(new Bundle {
        val op = Flipped(Decoupled(UInt((dataWidth + 3).W)))
        val head = Decoupled(UInt(log2Ceil(gq_cap).W))
    })
    val op = io.op.bits(2, 0)
    val data = io.op.bits(dataWidth + 2, 3)
    private val curr = RegInit(0.U(dataWidth.W))
    private val ptr = RegInit(0.U(log2Ceil(gq_cap).W))
    private val count = RegInit(0.U(log2Ceil(gq_cap).W))

    io.op.ready := true.B
    io.head.valid := true.B

    when(io.head.fire) {
        io.head.bits := ptr
    }.otherwise {
        io.head.bits := 0.U
    }

    when(io.op.fire) {
        when(op === 0.U) {
            curr := data
        }.elsewhen(op === 1.U) {
            ptr := data
        }.elsewhen(op === 2.U) {
            count := count + 1.U
        }.elsewhen(op === 3.U) {
            ptr := ptr + 1.U
            count := count - 1.U
        }.elsewhen(op === 4.U) {
            ptr := ptr + 1.U
        }.elsewhen(op === 5.U) {
            ptr := ptr - 1.U
        }
    }
}
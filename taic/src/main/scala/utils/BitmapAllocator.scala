package taic.utils

import chisel3._
import chisel3.util._

// BitmapAllocator: 用于分配队列资源，idx 表示队列的下标
// 硬件接口定义：
//      alloc 用于分配，从这个接口读出分配的下标
//      free 用于释放，向这个接口写入释放的下标
// n：支持分配的最大资源数量
class BitAllocator(val n: Int) extends Module {
    val io = IO(new Bundle {
        val alloc = Decoupled(UInt(log2Ceil(n).W))
        val free = Flipped(Decoupled(UInt(log2Ceil(n).W)))
        val alloc_count = Output((UInt((log2Ceil(n) + 1).W)))
        val full = Output(Bool())
        val alloced = Output(UInt(n.W))
    })

    private val bitmap = RegInit(VecInit(Seq.fill(n)(true.B)))
    io.alloced := ~Cat(bitmap.reverse)
    private val idx = PriorityEncoder(bitmap)
    io.alloc.bits := idx
    io.alloc.valid := true.B
    io.free.ready := true.B
    private val alloc_count = PopCount(~Cat(bitmap))
    io.alloc_count := alloc_count
    io.full := alloc_count === n.U

    when (io.alloc.fire) {
        bitmap(idx) := false.B
    }

    when (io.free.fire) {
        bitmap(io.free.bits) := true.B
    }
}
package taic.utils

import chisel3._
import chisel3.util._

// DataArray: 用于存储元素
// 硬件接口定义：
//      enq：入队
//      deq：出队
// n：支持的最大元素数量
class DataArray(val dataWidth: Int, val n: Int) extends Module {
    val io = IO(new Bundle {
        val idx = Input(UInt(dataWidth.W))
        val enq = Flipped(Decoupled(UInt((dataWidth).W)))
        val deq = Decoupled(UInt((dataWidth).W))
        val count = Output(UInt(dataWidth.W))
    })

    private val mem = RegInit(VecInit(Seq.fill(n)(0.U(dataWidth.W))))
    private val data = RegInit(0.U(dataWidth.W))
    private val s_idle :: s_enq :: s_enq_done :: s_deq :: s_deq_done :: s_error :: Nil = Enum(6)
    private val state = RegInit(s_idle)
    private val count = RegInit(0.U(dataWidth.W))

    io.enq.ready := state === s_enq_done || state === s_error
    io.deq.valid := state === s_deq_done || state === s_error
    io.deq.bits := data
    io.count := count

    // 入队
    when(state === s_idle && io.enq.valid) {
        data := io.enq.bits
        when(count === n.U) {
            state := s_error
        }.otherwise {
            state := s_enq
        }
    }

    // 出队
    when(state === s_idle && io.deq.ready) {
        when(count === 0.U) {
            data := 0.U
            state := s_error
        }.otherwise {
            state := s_deq
        }
    }

    private val new_mem = VecInit(Seq.fill(n)(0.U(dataWidth.W)))
    when(state === s_enq) {
        new_mem(0) := Mux(io.idx === 0.U, data, mem(0))
        for (i <- 1 until n) {
            when(i.U === io.idx) {
                new_mem(i) := data
            }.elsewhen(i.U > io.idx) {
                new_mem(i) := mem(i - 1)
            }.otherwise {
                new_mem(i) := mem(i)
            }
        }
        mem := new_mem
        state := s_enq_done
    }

    when(state === s_deq) {
        new_mem(n - 1) := 0.U
        when(io.idx === (n - 1).U) {
            data := mem(n - 1)
        }
        for (i <- 0 until n - 1) {
            when(i.U === io.idx) {
                new_mem(i) := mem(i + 1)
                data := mem(i)
            }.elsewhen(i.U > io.idx) {
                new_mem(i) := mem(i + 1)
            }.otherwise {
                new_mem(i) := mem(i)
            }
        }
        mem := new_mem
        state := s_deq_done
    }

    when(state === s_enq_done && io.enq.fire) {
        count := count + 1.U
        state := s_idle
    }

    when(state === s_deq_done && io.deq.fire) {
        count := count - 1.U
        state := s_idle
    }

    when(state === s_error && (io.enq.fire || io.deq.fire)) {
        state := s_idle
    }

}
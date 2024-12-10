package taic.utils

import chisel3._
import chisel3.util._

// ExtIntrSlots: 用于维护与外部中断相关的任务标识
// 硬件接口定义：
//      register：注册一个外部中断的处理任务
//      cancel：取消一个外部中断的处理任务
//      handlers：外部中断的处理任务
//      disable：在唤醒一次之后需要暂时关闭唤醒机制
//      enable：软件执行完外部中断的处理任务之后，需要调用相应的接口来重新使能唤醒机制
// n：支持分配的最大外部中断数量
class ExtIntrSlots(val n: Int, val dataWidth: Int) extends Module {
    val io = IO(new Bundle {
        val register = Vec(n, Flipped(Decoupled(UInt(dataWidth.W))))
        val wake_handler = Vec(n, Decoupled(UInt(dataWidth.W)))
    })

    private val handlers = RegInit(VecInit(Seq.fill(n)(0.U(dataWidth.W))))
    
    Seq.tabulate(n) { i =>
        io.register(i).ready := true.B
        io.wake_handler(i).valid := true.B
        io.wake_handler(i).bits := handlers(i)
        handlers(i) := Mux(io.register(i).fire, io.register(i).bits, Mux(io.wake_handler(i).fire, 0.U, handlers(i)))
    }
    

}
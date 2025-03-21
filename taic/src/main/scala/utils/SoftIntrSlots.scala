package taic.utils

import chisel3._
import chisel3.util._

// SoftIntrSlots: 用于维护与软件中断相关的任务标识
// 硬件接口定义：
//      register_sender：注册发送方
//      cancel_sender：取消发送方
//      register_receiver：注册接收方
//      wake_handler：唤醒接收任务
//      can_send：发送能力
//      can_recv：接收能力
// n：支持分配的最大软件中断数量
class SoftIntrSlots(val n: Int, val dataWidth: Int) extends Module {
    val io = IO(new Bundle {
        val register_sender = Flipped(Decoupled(UInt(dataWidth.W)))
        val cancel_sender = Flipped(Decoupled(UInt(dataWidth.W)))
        val register_receiver = Flipped(Decoupled(UInt(dataWidth.W)))
        val wake_handler = Vec(n, Decoupled(UInt(dataWidth.W)))
        val can_send = Vec(n, Output(UInt((dataWidth * 2).W)))
        val can_recv = Vec(n, Output(UInt((dataWidth * 2).W)))
        val clean = Flipped(Decoupled(Bool()))
    })

    // 发送能力，记录接收方的 os_proc
    private val sendcap = RegInit(VecInit(Seq.fill(n)(0.U((dataWidth * 2).W))))
    // 接收能力，记录发送方的 os_proc，以及自己的 handler
    private val recvcaps = RegInit(VecInit(Seq.fill(n)(0.U((dataWidth * 3).W))))

    private val s_idle :: s_reg_send0 :: s_reg_send1 :: s_reg_recv0 :: s_reg_recv1 :: s_reg_recv2 :: s_cancel_send0 :: s_cancel_send1 :: s_clean :: Nil = Enum(9)
    private val state = RegInit(s_idle)

    io.register_sender.ready := state === s_idle || state === s_reg_send0
    io.cancel_sender.ready := state === s_idle || state === s_cancel_send0
    io.register_receiver.ready := state === s_idle || state === s_reg_recv0 || state === s_reg_recv1

    private val os = RegInit(0.U(dataWidth.W))
    private val proc = RegInit(0.U(dataWidth.W))
    private val task = RegInit(0.U(dataWidth.W))

    private val send_matches = Cat(Seq.tabulate(n) { i => sendcap(i) === Cat(os, proc) }.reverse)
    private val send_matched_idx = PriorityEncoder(Cat(0.U, send_matches))
    private val send_emptys = Cat(Seq.tabulate(n) { i => sendcap(i) === 0.U }.reverse)
    private val send_empty_idx = PriorityEncoder(Cat(0.U, send_emptys))

    private val recv_matches = Cat(Seq.tabulate(n) { i => recvcaps(i)(dataWidth * 2 - 1, 0) === Cat(os, proc) }.reverse)
    private val recv_matched_idx = PriorityEncoder(Cat(0.U, recv_matches))
    private val recv_emptys = Cat(Seq.tabulate(n) { i => recvcaps(i) === 0.U }.reverse)
    private val recv_empty_idx = PriorityEncoder(Cat(0.U, recv_emptys))

    for (i <- 0 until n) {
        io.wake_handler(i).valid := true.B
        io.wake_handler(i).bits := recvcaps(i)(dataWidth * 3 - 1, dataWidth * 2)
        io.can_send(i) := sendcap(i)
        io.can_recv(i) := recvcaps(i)(dataWidth * 2 - 1, 0)
        sendcap(i) := Mux(state === s_clean, 0.U, Mux(state === s_reg_send1 && (send_empty_idx === i.U || send_matched_idx === i.U), Cat(os, proc), Mux(state === s_cancel_send1 && send_matched_idx === i.U, 0.U, sendcap(i))))
        recvcaps(i) := Mux(state === s_clean, 0.U, Mux(state === s_reg_recv2 && (recv_empty_idx === i.U || recv_matched_idx === i.U), Cat(task, os, proc), Mux(state === s_idle && io.wake_handler(i).fire, Mux(recvcaps(i)(dataWidth * 2 + 1) === 0.U, 0.U, recvcaps(i)), recvcaps(i))))
    }

    when(state === s_idle && io.register_sender.fire) {
        os := io.register_sender.bits
        state := s_reg_send0
    }
    when(state === s_reg_send0 && io.register_sender.fire) {
        proc := io.register_sender.bits
        state := s_reg_send1
    }
    when(state === s_reg_send1) {
        state := s_idle
    }

    when(state === s_idle && io.cancel_sender.fire) {
        os := io.cancel_sender.bits
        state := s_cancel_send0
    }
    when(state === s_cancel_send0 && io.cancel_sender.fire) {
        proc := io.cancel_sender.bits
        state := s_cancel_send1
    }
    when(state === s_cancel_send1) {
        state := s_idle
    }

    when(state === s_idle && io.register_receiver.fire) {
        os := io.register_receiver.bits
        state := s_reg_recv0
    }
    when(state === s_reg_recv0 && io.register_receiver.fire) {
        proc := io.register_receiver.bits
        state := s_reg_recv1
    }
    when(state === s_reg_recv1 && io.register_receiver.fire) {
        task := io.register_receiver.bits
        state := s_reg_recv2
    }
    when(state === s_reg_recv2) {
        state := s_idle
    }
    io.clean.ready := true.B
    when(io.clean.fire) {
        state := s_clean
    }
    when(state === s_clean) {
        state := s_idle
    }
}
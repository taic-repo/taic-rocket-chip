package taic.utils

import chisel3._
import chisel3.util._

// Controller: 控制器，用于分配队列资源，以及队列操作
// 
class Controller(val gq_num: Int, val lq_num: Int, val gq_cap: Int, val dataWidth: Int) extends Module {
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
    })

    // 申请时需要记录的信息
    private val os = RegInit(0.U(dataWidth.W))
    private val proc = RegInit(0.U(dataWidth.W))

    private val gq_allocator = Module(new BitAllocator(gq_num))
    private val alloced_gq = RegInit(0.U(log2Ceil(gq_num).W))
    // 这里的 gqs 还需要完善 GlobalQueue 的实现
    private val gqs = Seq.fill(gq_num)(Module(new GlobalQueue(dataWidth, lq_num, gq_cap)))

    private val s_idle :: s_wos :: s_wproc :: s_find :: s_alloc_gq :: s_gq_init :: s_alloc_lq :: s_ridx :: s_free_lq :: s_free_gq :: s_error :: Nil = Enum(11)

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
        }
        io.errors(i) <> gqs(i).io.error
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

}
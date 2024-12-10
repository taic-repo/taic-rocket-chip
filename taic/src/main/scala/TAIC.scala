package freechips.rocketchip.taic

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.interrupts._
import taic.utils._

object TAICConsts {
  def base: BigInt = 0x1000000
  def size = 0x1000000
  def gq_num = 4
  def lq_num = 2
  def gq_cap = 4
  def dataWidth = 64
  def lq_base = 0x1000
  def lq_size = 0x1000
}

case class TAICParams(baseAddress: BigInt = TAICConsts.base, intStages: Int = 0) {
  def address = AddressSet(baseAddress, TAICConsts.size - 1)
}

case object TAICKey extends Field[Option[TAICParams]](None)

case class TAICAttachParams(slaveWhere: TLBusWrapperLocation = CBUS)

case object TAICAttachKey extends Field(TAICAttachParams())

/** Asynchorous-Task-Scheduler-Interrupt Controller */
class TAIC(params: TAICParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("taic", Seq("riscv,taic")) {
    override val alwaysExtended: Boolean = true

    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val extra = Map("interrupt-controller" -> Nil, "#interrupt-cells" -> Seq(ResourceInt(1)))
      Description(name, mapping ++ extra)
    }
  }

  val node: TLRegisterNode = TLRegisterNode(
    address = Seq(params.address),
    device = device,
    beatBytes = beatBytes,
    concurrency = 1) // limiting concurrency handles RAW hazards on claim registers

  val intnode: IntNexusNode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false)

    def nDevices: Int = intnode.edges.in.map(_.source.num).sum

  lazy val module = new LazyModuleImp(this) {
    Annotated.params(this, params)

    // Compact the interrupt vector the same way
    // val interrupts = intnode.in.map { case (i, e) => i.take(e.source.num) }.flatten
    
    println(s"TAIC map ${nDevices} external interrupts:")


    val controller = Module(new Controller(TAICConsts.gq_num, TAICConsts.lq_num, TAICConsts.gq_cap, TAICConsts.dataWidth))
    val bitallocatorRegs = Seq(
      0x00 -> Seq(RegField(64, controller.io.alloced, controller.io.alloc)),
      0x08 -> Seq(RegField.w(64, controller.io.free)),
    )
    val enqRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size -> Seq(RegField.w(64, controller.io.enqs(i)))
    }
    val deqRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x08 -> Seq(RegField.r(64, controller.io.deqs(i)))
    }
    val errRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x10 -> Seq(RegField.r(64, controller.io.errors(i / TAICConsts.lq_num)))
    }
    // val queue = Module(new GlobalQueue(64, 2, 4))
    // val bitallocatorRegs = Seq(
    //   0x00 -> Seq(RegField.r(64, queue.io.alloc_lq)),
    //   0x08 -> Seq(RegField.w(64, queue.io.free_lq)),
    // )
    // val enqRegs = Seq.tabulate(2) { i =>
    //   0x10 + 8 * i -> Seq(RegField.w(64, queue.io.enqs(i)))
    // }
    // val deqRegs = Seq.tabulate(2) { i =>
    //   0x20 + 8 * i -> Seq(RegField.r(64, queue.io.deqs(i)))
    // }
    // val errRegs = Seq(
    //   0x30 -> Seq(RegField.r(64, queue.io.error))
    // )


    // node.regmap((deqReg ++ enqRegs ++ extintrRegs ++ simExtIntrRegs): _*)
    // node.regmap((bitallocatorRegs): _*)
    node.regmap((bitallocatorRegs ++ enqRegs ++ deqRegs ++ errRegs): _*)

  }
}

/** Trait that will connect a TAIC to a subsystem */
trait CanHavePeripheryTAIC {
  this: BaseSubsystem =>
  val taicOpt = p(TAICKey).map { params =>
    val tlbus = locateTLBusWrapper(p(TAICAttachKey).slaveWhere)
    val taic = LazyModule(new TAIC(params, cbus.beatBytes))
    taic.node := tlbus.coupleTo("taic") { TLFragmenter(tlbus) := _ }
    taic.intnode :=* ibus.toPLIC

    InModuleBody {
      taic.module.clock := tlbus.module.clock
      taic.module.reset := tlbus.module.reset
    }

    taic
  }
}
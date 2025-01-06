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
  def lq_num = 8
  def gq_cap = 64
  def dataWidth = 64
  def lq_base = 0x1000
  def lq_size = 0x1000
  def softintr_num = 6
}

class IpStatus extends Bundle {
  val has_intr = Bool()
  val hartid = UInt(TAICConsts.dataWidth.W)
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
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(2, Seq(Resource(device, "int"))))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false)

    def nDevices: Int = intnode.edges.in.map(_.source.num).sum

  lazy val module = new LazyModuleImp(this) {
    Annotated.params(this, params)

    // Compact the interrupt vector the same way
    val interrupts = intnode.in.map { case (i, e) => i.take(e.source.num) }.flatten
    val nTiles = intnode.out.size
    println(s"TAIC map ${nDevices} external interrupts, ${nTiles} Tiles")

    val controller = Module(new Controller(TAICConsts.gq_num, TAICConsts.lq_num, TAICConsts.gq_cap, TAICConsts.dataWidth, nDevices, TAICConsts.softintr_num))
    val ssip_status = Seq.tabulate(TAICConsts.gq_num) { i =>
      val ssip = controller.io.ssips(i)
      val hartid = controller.io.hartids(i)
      val ip_status = Wire(new IpStatus)
      ip_status.has_intr := ssip
      ip_status.hartid := hartid
      ip_status
    }
    val usip_status = Seq.tabulate(TAICConsts.gq_num) { i =>
      val usip = controller.io.usips(i)
      val hartid = controller.io.hartids(i)
      val ip_status = Wire(new IpStatus)
      ip_status.has_intr := usip
      ip_status.hartid := hartid
      ip_status
    }
    
    val core_ssips = Seq.fill(nTiles) { RegInit(0.U) }
    core_ssips.zipWithIndex.foreach { case (hart, i) =>
      hart := ssip_status.map(x => x.has_intr && x.hartid === i.asUInt).reduce(_ || _)
    }
    val core_usips = Seq.fill(nTiles) { RegInit(0.U) }
    core_usips.zipWithIndex.foreach { case (hart, i) =>
      hart := usip_status.map(x => x.has_intr && x.hartid === i.asUInt).reduce(_ || _)
    }
    
    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
      int(0) := ShiftRegister(core_ssips(i)(0), params.intStages) // ssip
      int(1) := ShiftRegister(core_usips(i)(0), params.intStages) // usip
    }
    
    val simExtIntr = Seq.fill(nDevices)(RegInit(false.B))
    Seq.tabulate(nDevices) { i =>
      controller.io.ext_intrs(i) := Mux(interrupts(i), interrupts(i), simExtIntr(i))
    }
    val bitallocatorRegs = Seq(
      0x00 -> Seq(RegField(TAICConsts.dataWidth, controller.io.alloced, controller.io.alloc)),
      0x08 -> Seq(RegField.w(TAICConsts.dataWidth, controller.io.free)),
    )
    val enqRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size -> Seq(RegField.w(TAICConsts.dataWidth, controller.io.enqs(i)))
    }
    val deqRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x08 -> Seq(RegField.r(TAICConsts.dataWidth, controller.io.deqs(i)))
    }
    val errRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x10 -> Seq(RegField.r(TAICConsts.dataWidth, controller.io.errors(i / TAICConsts.lq_num)))
    }
    val senderRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x18 -> Seq(RegField.w(TAICConsts.dataWidth, controller.io.register_sender(i)))
    }
    val cancelRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x20 -> Seq(RegField.w(TAICConsts.dataWidth, controller.io.cancel_sender(i)))
    }
    val receiverRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x28 -> Seq(RegField.w(TAICConsts.dataWidth, controller.io.register_receiver(i)))
    }
    val sendIntrRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x30 -> Seq(RegField.w(TAICConsts.dataWidth, controller.io.send_intr(i)))
    }
    val whartidRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      TAICConsts.lq_base + i * TAICConsts.lq_size + 0x38 -> Seq(RegField.w(TAICConsts.dataWidth, controller.io.whartid(i)))
    }
    val extintrRegs = Seq.tabulate(TAICConsts.gq_num * TAICConsts.lq_num) { i =>
      Seq.tabulate(nDevices) { j =>
        TAICConsts.lq_base + i * TAICConsts.lq_size + 0x40 + 8 * j -> Seq(RegField.w(TAICConsts.dataWidth, controller.io.register_ext_intr(i * nDevices + j)))
      }
    }.flatten
    val simExtIntrRegs = Seq.tabulate(nDevices) { i =>
      0x10 + 0x08 * i -> Seq(RegField.w(TAICConsts.dataWidth, RegWriteFn { (valid, data) =>
        simExtIntr(i) := valid
        true.B
      }))
    }

    node.regmap((bitallocatorRegs ++ enqRegs ++ deqRegs ++ errRegs ++ senderRegs ++ cancelRegs ++ receiverRegs ++ sendIntrRegs ++ whartidRegs ++ extintrRegs ++ simExtIntrRegs): _*)

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
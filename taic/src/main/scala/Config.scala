package freechips.rocketchip.taic

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import freechips.rocketchip.devices.tilelink._

class WithCustomBootROM(resetAddress: BigInt, bootImgPath: String)
  extends Config((_, _, up) => { case BootROMLocated(x) =>
    up(BootROMLocated(x)).map(_.copy(hang = resetAddress, contentFileName = bootImgPath))
  })

class WithTAIC extends Config((_, _, _) => {
  case TAICKey => Some(TAICParams())
})

class TaicConfig extends Config(
  new WithNBigCores(2) ++
    new WithNExtTopInterrupts(6) ++
    new WithTimebase((BigInt(10000000))) ++ // 10 MHz
    new WithDTS("freechips.rocketchip-unknown", Nil) ++
    new WithCustomBootROM(0x10000, "../bootrom/bootrom.img") ++
    new WithDefaultMemPort ++
    new WithDefaultMMIOPort ++
    new WithDefaultSlavePort ++
    new WithoutTLMonitors ++
    new WithCoherentBusTopology ++
    new BaseSubsystemConfig ++ 
    new WithTAIC 
)

class TaicTestConfig extends Config(new DefaultConfig )
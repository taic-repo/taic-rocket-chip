package freechips.rocketchip.taic

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

class WithCustomBootROM(resetAddress: BigInt, bootImgPath: String)
  extends Config((_, _, up) => { case BootROMLocated(x) =>
    up(BootROMLocated(x)).map(_.copy(hang = resetAddress, contentFileName = bootImgPath))
  })

class WithTAIC extends Config((_, _, _) => {
  case TAICKey => Some(TAICParams())
})

class WithNEBigCores(n: Int, overrideIdOffset: Option[Int] = None) extends Config((site, here, up) => {
  case RocketTilesKey => {
    val prev = up(RocketTilesKey, site)
    val idOffset = overrideIdOffset.getOrElse(prev.size)
    val big = RocketTileParams(
      core   = RocketCoreParams(
        useNE = true,
        useSupervisor = true,
        mulDiv = Some(MulDivParams(
        mulUnroll = 8,
        mulEarlyOut = true,
        divEarlyOut = true))),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        blockBytes = site(CacheBlockBytes))))
    List.tabulate(n)(i => big.copy(hartId = i + idOffset)) ++ prev
  }
})

class TaicConfig extends Config(
  new WithNEBigCores(4) ++
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
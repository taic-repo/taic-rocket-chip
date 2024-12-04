package freechips.rocketchip.taic

import firrtl.options.StageMain
import freechips.rocketchip.system.RocketChipStage

object Generator extends StageMain(new RocketChipStage)
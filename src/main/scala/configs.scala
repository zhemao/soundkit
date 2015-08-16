package soundkit

import Chisel._

class DefaultConfig extends ChiselConfig(
  topDefinitions = { (pname, site, here) =>
    pname match {
      case AudioSampleWidth => 16
      case AudioBclkDivide => 4
      case AudioLrckDivide => 256
      case AudioNChannels => 1
    }
  })

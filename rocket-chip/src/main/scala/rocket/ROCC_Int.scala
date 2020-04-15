// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

import Instructions._

class ROCC_Int(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val in2 = UInt(INPUT, xLen)
    val in1 = UInt(INPUT, xLen)
    val out = UInt(OUTPUT, xLen)
    val adder_out = UInt(OUTPUT, xLen)
    val cmp_out = Bool(OUTPUT)
  }

  val adder = io.in1 + io.in2;
  io.cmp_out := Mux(adder === UInt(2021) , Bool(true) , Bool(false))

}

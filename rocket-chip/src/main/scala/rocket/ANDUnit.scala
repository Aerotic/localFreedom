// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

import Instructions._

import ALU._


class ADDU(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val fn = Bits(INPUT, SZ_ALU_FN)
    val in2 = UInt(INPUT, xLen)
    val in1 = UInt(INPUT, xLen)
    val out = UInt(OUTPUT, xLen)
  }
  
  val testWire =Reg(Flipped(Decoupled(UInt(xLen))))
  testWire.valid := 1.U
  testWire.bits := 2.U
  val q = Module(new Queue(UInt(xLen), 3))
  q.io.enq <> testWire

  io.out := io.in2+io.in1+isSub(io.fn)

}

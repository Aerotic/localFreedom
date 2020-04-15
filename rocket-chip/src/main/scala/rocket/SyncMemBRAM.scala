package freechips.rocketchip.rocket
import chisel3._
import chisel3.util._
class Banks(val nBanks: Int, val nElem: Int, val elemW: Int) extends Module {
  val io = IO(new Bundle {
    val dataIn  = Input(Vec(nBanks, UInt(elemW.W)))
    val addr    = Input(Vec(nBanks, UInt(log2Up(nElem).W)))
    val ren     = Input(Vec(nBanks, Bool()))
    val wen     = Input(Vec(nBanks, Bool()))
    val dataOut = Output(Vec(nBanks, UInt(elemW.W)))
  })

  val memory = Seq.fill(nBanks)(SyncReadMem(nElem, UInt(elemW.W)))
 
  for (i <- 0 until nBanks){
    val bank = memory(i)
    when(io.wen(i.U)){bank.write(io.addr(i.U), io.dataIn(i.U))}
    io.dataOut(i.U) := bank.read(io.addr(i.U), io.ren(i.U))
  }

}

class BankedMemory(val nBanks: Int, val nElem: Int, val elemW: Int) {
  val memory = Module(new Banks(nBanks, nElem, elemW))
  for (i <- 0 until nBanks){
    memory.io.dataIn(i.U) := 0.U
    memory.io.addr(i.U)   := 0.U
    memory.io.ren(i.U)    := false.B
    memory.io.wen(i.U)    := false.B
  }

  def read(bank: UInt, addr: UInt, en: Bool): UInt = {
    memory.io.addr(bank)  := addr
    memory.io.ren(bank)   := en
    memory.io.dataOut(bank)
  }

  def write(bank: UInt, addr: UInt, data: UInt): Unit = {
    memory.io.addr(bank)    := addr
    memory.io.dataIn(bank)  := data
    memory.io.wen(bank)     := true.B
  }
}

/* SyncReadMemory banks */
object BankedMemory {
  def apply(nBanks: Int, nElems: Int, elemW: Int): BankedMemory = {
    new BankedMemory(nBanks, nElems, elemW)
  }
}
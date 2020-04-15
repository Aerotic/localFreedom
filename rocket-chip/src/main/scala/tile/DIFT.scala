package freechips.rocketchip.tile

import Chisel._

class InstructionInfo extends Bundle{
  //control signals
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val mem = Bool()

  //instruction info
  val rs2 = Bits(width = 5)
  val rs1 = Bits(width = 5)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val rd = Bits(width = 5)
  val pc = UInt(64.W)
  val memAddr = Bits(width = 32)
}

class TagInfo extends Bundle {
  val rs1Index = Bits(width = 5)
  val rs2Index = Bits(width = 5)
  val rdIndex = Bits(width = 5)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val pc = UInt(64.W)
  val xPC = Bool()
  val propagationRule = Bits(width = 2) //00:No-operation,01:move-rule,10:OR-rule,11:AND-rule,
  val checkRule = Bits(width = 2) //00:No-check,01:PC-check
}

class RegisterTaintTable(nReg:Int, widthOfTag:Int = 1) {

  val regTaintTable = Mem(nReg, UInt(1.W))

  def taintRead(rAddr:UInt) = {
    regTaintTable(rAddr)
  }

  def taintWrite(rAddr:UInt, taint:UInt) = {
    regTaintTable(rAddr) := taint
  }
}

class InstAnalyzeMod extends Module{
  val io = IO(new Bundle{
    val info = Decoupled(new InstructionInfo).flip()
    // val infoValid = Bool(Input)
    val infoValid = Bool().asInput
    val tagInfo = Decoupled(new TagInfo)
  })

  //queue is not empty, then we can receive info from queue
  io.info.ready := io.info.valid

  //tag info is valid
  io.tagInfo.valid := io.info.valid

  printf("rs1:%d(xs1:%d)--rs2:%d(xs2:%d)--rd:%d(xd:%d)--branch:%d--jalr:%d--mem:%d\n", io.info.bits.rs1, io.info.bits.xs1,
    io.info.bits.rs2, io.info.bits.xs2, io.info.bits.rd, io.info.bits.xd, io.info.bits.branch, io.info.bits.jalr, io.info.bits.mem)

  when(io.info.valid) {
    io.tagInfo.bits.rs1Index := io.info.bits.rs1
    io.tagInfo.bits.rs2Index := io.info.bits.rs2
    io.tagInfo.bits.rdIndex := io.info.bits.rd
    io.tagInfo.bits.xs1 := io.info.bits.xs1
    io.tagInfo.bits.xs2 := io.info.bits.xs2
    io.tagInfo.bits.xd := io.info.bits.xd
    io.tagInfo.bits.pc := io.info.bits.pc
    io.tagInfo.bits.xPC := io.info.bits.jalr

    when(io.info.bits.branch) {//branch instruction
      io.tagInfo.bits.propagationRule := 0.asUInt() //no propagation
      io.tagInfo.bits.checkRule := 0.asUInt() //no check
    } .elsewhen(io.info.bits.jalr) {//jump instruction
      io.tagInfo.bits.propagationRule := 1.asUInt() //move propagation
      io.tagInfo.bits.checkRule := 1.asUInt() //PC-check
    } .elsewhen(io.info.bits.mem) {//memory instruction
      io.tagInfo.bits.propagationRule := 1.asUInt() //move propagation
      io.tagInfo.bits.checkRule := 0.asUInt() //No-check
    } .otherwise {//ALU instruction
      io.tagInfo.bits.checkRule := 0.asUInt() //No-check

      //imm-ALU
      when(io.info.bits.xs1 && io.info.bits.xd && !io.info.bits.xs2) {
        io.tagInfo.bits.propagationRule := 1.asUInt() //move propagation
      }

      //reg-ALU
      when(io.info.bits.xs1 && io.info.bits.xd && io.info.bits.xs2) {
        io.tagInfo.bits.propagationRule := 2.asUInt() //OR propagation
      }
    }
  }

}
// val rdTaint = UInt(widthOfTag.W).asInput
// val rdTaint = Input(UInt(widthOfTag.W))
// val PCTaint = Input(UInt(widthOfTag.W))
// val rdValid = Input(Bool())
// val PCValid = Input(Bool())
// val checkRule = Input(UInt(cRule.W))
// val exception = Output(UInt(1.W))
class TagCheck (widthOfTag:Int = 1, cRule:Int = 2) extends Module{
  val io = IO(new Bundle{
    val rdTaint = UInt(widthOfTag.W).asInput
    val PCTaint = (UInt(widthOfTag.W)).asInput
    val rdValid = (Bool()).asInput
    val PCValid = (Bool()).asInput
    val checkRule =(UInt(cRule.W)).asInput
    val exception = (UInt(1.W)).asOutput
  })

  io.exception := 0.asUInt()

  switch(io.checkRule){
    is (0.U) {//No-check
      io.exception := 0.asUInt()
    }
    is (1.U) {//PC-check
      io.exception := io.PCValid & (io.PCTaint =/= 0.asUInt())
    }
  }
  //printf("rule:%d--rdTaint:%d--rdValid:%d--PCTaint:%d--PCValid:%d\n", io.checkRule, io.rdTaint, io.rdValid, io.PCTaint, io.PCValid)
  printf("exception:%d\n", io.exception)
}

// val tagInfo = Flipped(Decoupled(new TagInfo)) //come from TagPropagation
// val rs1Taint = Input(UInt(widthOfTag.W))  //come from TagPropagation
// val rs2Taint = Input(UInt(widthOfTag.W))  //come from TagPropagation
// val rdTaint = Output(UInt(widthOfTag.W))  //output to TagLUT and TagCheck
// val rdAddr = Output(UInt(widthOfReg.W)) //output to TagLUT
// val rdValid = Output(Bool())  //output to TagLUT and TagCheck
// val PCTaint = Output(UInt(widthOfTag))
// val PCValid = Output(Bool())  //output to TagCheck
// val checkRule = Output(UInt(cRule.W)) //output to TagCheck
class TagPropagation(widthOfTag:Int = 1, pRule:Int = 2, cRule:Int = 2, widthOfReg:Int = 5) extends Module{
  val io = IO(new Bundle{
    val tagInfo = Flipped(Decoupled(new TagInfo)) //come from TagPropagation
    val rs1Taint =(UInt(widthOfTag.W)).asInput  //come from TagPropagation
    val rs2Taint =(UInt(widthOfTag.W)).asInput  //come from TagPropagation
    val rdTaint = (UInt(widthOfTag.W)).asOutput  //output to TagLUT and TagCheck
    val rdAddr = (UInt(widthOfReg.W)).asOutput //output to TagLUT
    val rdValid = (Bool()).asOutput  //output to TagLUT and TagCheck
    val PCTaint = (UInt(widthOfTag)).asOutput
    val PCValid = (Bool()).asOutput  //output to TagCheck
    val checkRule =(UInt(cRule.W)).asOutput//output to TagCheck
  })

  io.rdTaint := Mux(!io.tagInfo.valid, 0.asUInt(),
    Mux(io.tagInfo.bits.propagationRule === 0.asUInt(), 0.asUInt(),
      Mux(io.tagInfo.bits.propagationRule === 1.asUInt(), io.rs1Taint,
        Mux(io.tagInfo.bits.propagationRule === 2.asUInt(), io.rs1Taint | io.rs2Taint,
          Mux(io.tagInfo.bits.propagationRule === 3.asUInt(), io.rs1Taint & io.rs2Taint, 0.asUInt())))))

  io.rdAddr := Mux(io.tagInfo.valid & io.tagInfo.bits.xd, io.tagInfo.bits.rdIndex, 0.asUInt())
  io.rdValid := io.tagInfo.valid & io.tagInfo.bits.xd

  io.checkRule := io.tagInfo.bits.checkRule

  io.PCValid := io.tagInfo.bits.xPC
  io.PCTaint := io.rs1Taint

  //printf("valid:%d--xd:%d--rs1Taint:%d--rs2Taint:%d--rdTaint:%d\n", io.tagInfo.valid, io.tagInfo.bits.xd, io.rs1Taint, io.rs2Taint, io.rdTaint)
  printf("rdIndex:%d\n", io.tagInfo.bits.rdIndex)
}
    // val tagInfo = Flipped(Decoupled(new TagInfo)) //come from InstAnalyzeMod
    // val rs1Taint = Output(UInt(widthOfTag.W)) //output to TagPropagation
    // val rs2Taint = Output(UInt(widthOfTag.W)) //output to TagPropagation
    // val tagPropagationInfo = Decoupled(new TagInfo) //output to TagPropagation
    // val rdTaint = Input(UInt(widthOfTag.W)) //come from TagPropagation
    // val rdAddr = Input(UInt(widthOfReg.W))  //come from TagPropagation
    // val rdValid = Input(Bool()) //come from TagPropagation
class TagLUT (widthOfTag:Int = 1, pRule:Int = 2, cRule:Int = 2, widthOfReg:Int = 5) extends Module{
  val io = IO(new Bundle{
    val tagInfo = Flipped(Decoupled(new TagInfo))
    val rs1Taint = (UInt(widthOfTag.W)).asOutput
    val rs2Taint = (UInt(widthOfTag.W)).asOutput
    val tagPropagationInfo = Decoupled(new TagInfo)
    val rdTaint = (UInt(widthOfTag.W)).asInput
    val rdAddr = (UInt(widthOfReg.W)).asInput
    val rdValid = (Bool()).asInput
  })

  val regTaintTable = new RegisterTaintTable(32)

  io.tagPropagationInfo <> io.tagInfo

  io.tagPropagationInfo.valid := io.tagInfo.valid

  io.rs1Taint := Mux(io.tagInfo.valid,
    regTaintTable.taintRead(io.tagInfo.bits.rs1Index),
    0.asUInt())

  io.rs2Taint := Mux(io.tagInfo.valid,
    regTaintTable.taintRead(io.tagInfo.bits.rs2Index),
    0.asUInt())

  when(io.rdValid) {
    regTaintTable.taintWrite(io.rdAddr, io.rdTaint)
    /*for(i <- 0 to 31) {
      printf("reg%d--%d\n", i.asUInt(), regTaintTable.taintRead(i.asUInt()))
    }*/
  }

  //printf("rdValid:%d--rdAddr:%d--rdTaint:%d\n", io.rdValid, io.rdAddr, io.rdTaint)
  //printf("read tag----valid:%d\n", io.tagInfo.valid)
  //printf("read tag----rs1Taint:%d--rs2Taint:%d\n", io.rs1Taint, io.rs2Taint)

}

class DIFTCoprocessorTop(nQueueEntry:Int = 6) extends Module{
  val io = IO(new Bundle{
    val diftInfo = Flipped(Decoupled(new InstructionInfo))
  })

  //instruction info queue
  val queue = Module(new Queue(new InstructionInfo, nQueueEntry))
  queue.io.enq <> io.diftInfo

  //instruction info analyze module
  val instAnalyze = Module(new InstAnalyzeMod)
  instAnalyze.io.info <> queue.io.deq
  //queue is not empty and can send info to instruction info analyze module
  instAnalyze.io.infoValid := Mux(queue.io.count > 0.asUInt(), true.B, false.B)

  //tag look up table
  val tagLUT = Module(new TagLUT)
  tagLUT.io.tagInfo <> instAnalyze.io.tagInfo

  //tag propagation module
  val tagPropagation = Module(new TagPropagation)
  tagPropagation.io.tagInfo <> tagLUT.io.tagPropagationInfo
  tagPropagation.io.rs1Taint := tagLUT.io.rs1Taint
  tagPropagation.io.rs2Taint := tagLUT.io.rs2Taint
  tagLUT.io.rdValid := tagPropagation.io.rdValid
  tagLUT.io.rdAddr := tagPropagation.io.rdAddr
  tagLUT.io.rdTaint := tagPropagation.io.rdTaint

  val tagCheck = Module(new TagCheck)
  tagCheck.io.rdTaint := tagPropagation.io.rdTaint
  tagCheck.io.rdValid := tagPropagation.io.rdValid
  tagCheck.io.PCTaint := tagPropagation.io.PCTaint
  tagCheck.io.PCValid := tagPropagation.io.PCValid
  tagCheck.io.checkRule := tagPropagation.io.checkRule
}

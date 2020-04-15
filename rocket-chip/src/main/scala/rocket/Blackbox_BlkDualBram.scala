package freechips.rocketchip.rocket
import chisel3._
import chisel3.util._
class Blackbox_BlkDualBram extends BlackBox with HasBlackBoxResource{
    val io = IO(new Bundle {
        val clka = Input(Clock()) // 这个名必须与verilog中的名字一致
        val ena = Input(UInt(1.W)) // 这个名必须与verilog中的名字一致
        val wea = Input(UInt(1.W)) // 这个名必须与verilog中的名字一致
        val addra = Input(UInt(10.W))
        val dina = Input(UInt(32.W))
        val douta = Output(UInt(32.W))

        val clkb = Input(Clock()) // 这个名必须与verilog中的名字一致
        val enb = Input(UInt(1.W)) // 这个名必须与verilog中的名字一致
        val web = Input(UInt(1.W)) // 这个名必须与verilog中的名字一致
        val addrb = Input(UInt(10.W))
        val dinb = Input(UInt(32.W))
        val doutb = Output(UInt(32.W))
    })
    setResource("/ipwrapper/Blackbox_BlkDualBram.v")
}

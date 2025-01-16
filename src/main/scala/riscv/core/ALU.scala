// mycpu is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import riscv.Parameters

object ALUFunctions extends ChiselEnum {
  val zero, add, sub, sll, slt, xor, or, and, srl, sra, sltu, mul, mulh, mulhsu, mulhum, div, divu, rem, remu = Value
}

class ALU extends Module {
  val io = IO(new Bundle {
    val func = Input(ALUFunctions())

    val op1 = Input(UInt(Parameters.DataWidth))
    val op2 = Input(UInt(Parameters.DataWidth))

    val result = Output(UInt(Parameters.DataWidth))
  })

  // 預設結果為 0
  io.result := 0.U

  switch(io.func) {
    is(ALUFunctions.add) {
      io.result := io.op1 + io.op2
    }
    is(ALUFunctions.sub) {
      io.result := io.op1 - io.op2
    }
    is(ALUFunctions.sll) {
      io.result := io.op1 << io.op2(4, 0)
    }
    is(ALUFunctions.slt) {
      io.result := Mux(io.op1.asSInt < io.op2.asSInt, 1.U, 0.U)
    }
    is(ALUFunctions.xor) {
      io.result := io.op1 ^ io.op2
    }
    is(ALUFunctions.or) {
      io.result := io.op1 | io.op2
    }
    is(ALUFunctions.and) {
      io.result := io.op1 & io.op2
    }
    is(ALUFunctions.srl) {
      io.result := io.op1 >> io.op2(4, 0)
    }
    is(ALUFunctions.sra) {
      io.result := (io.op1.asSInt >> io.op2(4, 0)).asUInt
    }
    is(ALUFunctions.sltu) {
      io.result := Mux(io.op1 < io.op2, 1.U, 0.U)
    }

    // M-extension operations
    is(ALUFunctions.mul) {
      io.result := (io.op1 * io.op2)(31, 0) 
    }
    is(ALUFunctions.mulh) {
      io.result := (io.op1.asSInt * io.op2.asSInt)(63, 32).asUInt 
    }
    is(ALUFunctions.mulhsu) {
      io.result := (io.op1.asSInt * io.op2)(63, 32).asUInt
    }
    is(ALUFunctions.mulhum) {
      io.result := (io.op1 * io.op2)(63, 32) 
    }
    is(ALUFunctions.div) {
      when(io.op2 === 0.U) {
        io.result := 0.U
      }.otherwise {
        io.result := (io.op1.asSInt / io.op2.asSInt).asUInt
      }
    }
    is(ALUFunctions.divu) {
      when(io.op2 === 0.U) {
        io.result := 0.U 
      }.otherwise {
        io.result := io.op1 / io.op2
      }
    }
    is(ALUFunctions.rem) {
      when(io.op2 === 0.U) {
        io.result := io.op1
      }.otherwise {
        io.result := (io.op1.asSInt % io.op2.asSInt).asUInt
      }
    }
    is(ALUFunctions.remu) {
      when(io.op2 === 0.U) {
        io.result := io.op1
      }.otherwise {
        io.result := io.op1 % io.op2
      }
    }
  }
}

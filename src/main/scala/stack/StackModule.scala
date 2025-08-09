//package stack
//
//import chisel3._
//import chisel3.util._
//import chisel3.stage.ChiselStage
//import java.nio.file.Paths
//
//class StackModule(val dataWidth: Int, val len: Int) extends Module {
//  val io = IO(new Bundle {
//    val in        = Input(UInt(32.W))             // instruction
//    val out       = Output(UInt(dataWidth.W))     // data out
//    val underflow = Output(Bool())
//    val overflow  = Output(Bool())
//    val isEmpty   = Output(Bool())
//    val isFull    = Output(Bool())
//    val popped    = Output(Bool())
//    val peeked    = Output(Bool())
//  })
//
//  // ------------------------------------------------------------------
//  // 1) Use a Mem for the stack storage
//  // ------------------------------------------------------------------
//  val stack = Mem(len, UInt(dataWidth.W))
//
//  // ------------------------------------------------------------------
//  // 2) A single register for SP (points at next free slot),
//  //    reset to 0
//  // ------------------------------------------------------------------
//  val sp = RegInit(0.U(log2Ceil(len + 1).W))
//
//  // ------------------------------------------------------------------
//  // Decode the 32‑bit “instruction”
//  // ------------------------------------------------------------------
//  val opcode = io.in(6, 0)
//  val imm    = io.in(31, 7)
//  val push   = opcode === "b0100111".U
//  val pop    = opcode === "b1000011".U
//  val peek   = opcode === "b1000000".U
//
//  // convenience wires
//  val isEmptyReg = sp === 0.U
//  val isFullReg  = sp === len.U
//
//  io.isEmpty := isEmptyReg
//  io.isFull  := isFullReg
//
//  // default outputs
//  io.out       := 0.U
//  io.underflow := false.B
//  io.overflow  := false.B
//  io.popped    := false.B
//  io.peeked    := false.B
//
//  // ------------------------------------------------------------------
//  // Push: write at address sp, then increment
//  // ------------------------------------------------------------------
//  when(push) {
//    when(!isFullReg) {
//      stack.write(sp, imm.pad(dataWidth)(dataWidth-1,0))
//      sp := sp + 1.U
//    }.otherwise {
//      io.overflow := true.B
//    }
//  }
//
//  // ------------------------------------------------------------------
//  // Pop: decrement sp, then read
//  // ------------------------------------------------------------------
//  when(pop) {
//    when(!isEmptyReg) {
//      // compute new top index
//      val newSp = sp - 1.U
//      sp := newSp
//      // combinational read from Mem
//      io.out := stack.read(newSp)
//      io.popped := true.B
//    }.otherwise {
//      io.underflow := true.B
//    }
//  }
//
//  // ------------------------------------------------------------------
//  // Peek: read at sp-1 without changing pointer
//  // ------------------------------------------------------------------
//  when(peek) {
//    when(!isEmptyReg) {
//      io.out    := stack.read(sp - 1.U)
//      io.peeked := true.B
//    }.otherwise {
//      io.underflow := true.B
//    }
//  }
//}

package stack

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import java.nio.file.Paths

class StackModule(dataWidth: Int, len: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt(dataWidth.W))
    val underflow = Output(Bool())
    val overflow = Output(Bool())
    val isEmpty = Output(Bool())
    val isFull = Output(Bool())
    val popped = Output(Bool())
    val peeked = Output(Bool())
  })

  // Stack storage using a register file
  val stack = Mem(len, UInt(dataWidth.W))
  val stackPtr = RegInit(0.U(log2Ceil(len + 1).W))
  
  // Instruction decoding
  val opcode = io.in(6, 0)
  val imm = io.in(31, 7)
  val isPush = opcode === "b0100111".U
  val isPop = opcode === "b1000011".U
  val isPeek = opcode === "b1000000".U

  // Registered outputs
  val reg_out = RegInit(0.U(dataWidth.W))
  val reg_underflow = RegInit(false.B)
  val reg_overflow = RegInit(false.B)
  val reg_popped = RegInit(false.B)
  val reg_peeked = RegInit(false.B)

  io.out := reg_out
  io.underflow := reg_underflow
  io.overflow := reg_overflow
  io.popped := reg_popped
  io.peeked := reg_peeked
  io.isEmpty := stackPtr === 0.U
  io.isFull := stackPtr === len.U

  // Default registered outputs
  reg_out := 0.U(dataWidth.W)
  reg_underflow := false.B
  reg_overflow := false.B
  reg_popped := false.B
  reg_peeked := false.B

  // Handle instructions
  when(isPush) {
    when(!io.isFull) {
      // Push the zero-extended immediate value
      val pushValue = if (dataWidth <= 25) {
        imm(dataWidth - 1, 0)
      } else {
        Cat(0.U((dataWidth - 25).W), imm)
      }
      stack(stackPtr) := pushValue
      stackPtr := stackPtr + 1.U
    }.otherwise {
      reg_overflow := true.B
    }
  }.elsewhen(isPop) {
    when(!io.isEmpty) {
      // Pop the top value
      reg_out := stack(stackPtr - 1.U)
      stackPtr := stackPtr - 1.U
      reg_popped := true.B
    }.otherwise {
      reg_underflow := true.B
      reg_out := 0.U(dataWidth.W)
    }
  }.elsewhen(isPeek) {
    when(!io.isEmpty) {
      // Peek the top value
      reg_out := stack(stackPtr - 1.U)
      reg_peeked := true.B
    }.otherwise {
      reg_underflow := true.B
      reg_out := 0.U(dataWidth.W)
    }
  }

  // Reset handling
  when(reset.asBool) {
    stackPtr := 0.U
    for (i <- 0 until len) {
      stack(i) := 0.U(dataWidth.W)
    }
    reg_out := 0.U(dataWidth.W)
    reg_underflow := false.B
    reg_overflow := false.B
    reg_popped := false.B
    reg_peeked := false.B
  }
}

object SVGen extends App {
  val out = Paths.get(
    "out",
    this.getClass
      .getName
      .stripSuffix("$")
  ).toString
  new ChiselStage().emitSystemVerilog(
    new StackModule(args(0).toInt, args(1).toInt),
    Array("--target-dir", out),
  )
}



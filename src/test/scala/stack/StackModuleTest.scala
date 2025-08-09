package stack

import scala.util.Random
import scala.collection.mutable.Stack
import scala.math.pow

import chisel3._
import org.scalatest.freespec.AnyFreeSpec
import chiseltest._

class StackModuleTest extends AnyFreeSpec with ChiselScalatestTester {
  def pushToStack(
    random: Random,
    dataWidth: Long,
    len: Int,
    stack: Stack[Long],
    s: StackModule
  ) = {
    val push_val = random.between(0, pow(2, (if (dataWidth < 32) dataWidth else 25).toDouble).toLong)
    s.io.in.poke(
      ("b" + String.format(
        "%" + 25 + "s", push_val.toBinaryString
      ).replace(" ", "0") + "0100111").U
    )
    s.clock.step(1)
    s.io.out.expect(0.U)
    s.io.underflow.expect(0.B)
    s.io.popped.expect(0.B)
    s.io.peeked.expect(0.B)
    if (stack.length < len) {
      stack.push(push_val)
      s.io.overflow.expect(0.B)
    } else {
      s.io.overflow.expect(1.B)
    }
    s.io.isEmpty.expect(stack.isEmpty.B)
    s.io.isFull.expect((stack.length == len).B)
  }

  def popFromStack(
    random: Random,
    dataWidth: Long,
    len: Int,
    stack: Stack[Long],
    s: StackModule
  ) = {
    var pop_val = 0L
    s.io.in.poke(
      ("b" + String.format(
        "%" + 25 + "s", 0.toBinaryString
      ).replace(" ", "0") + "1000011").U
    )
    s.clock.step(1)
    s.io.overflow.expect(0.B)
    s.io.peeked.expect(0.B)
    if (!stack.isEmpty) {
      pop_val = stack.pop()
      s.io.underflow.expect(0.B)
      s.io.popped.expect(1.B)
    } else {
      s.io.underflow.expect(1.B)
      s.io.popped.expect(0.B)
    }
    s.io.out.expect(pop_val.U)
    s.io.isEmpty.expect(stack.isEmpty.B)
    s.io.isFull.expect((stack.length == len).B)
  }

  def peekAtStack(
    random: Random,
    dataWidth: Long,
    len: Int,
    stack: Stack[Long],
    s: StackModule
  ) = {
    var peek_val = 0L
    s.io.in.poke(
      ("b" + String.format(
        "%" + 25 + "s", 0.toBinaryString
      ).replace(" ", "0") + "1000000").U
    )
    s.clock.step(1)
    s.io.overflow.expect(0.B)
    s.io.popped.expect(0.B)
    if (!stack.isEmpty) {
      peek_val = stack.top
      s.io.underflow.expect(0.B)
      s.io.peeked.expect(1.B)
    } else {
      s.io.underflow.expect(1.B)
      s.io.peeked.expect(0.B)
    }
    s.io.out.expect(peek_val.U)
    s.io.isEmpty.expect(stack.isEmpty.B)
    s.io.isFull.expect((stack.length == len).B)
  }

  "StackModule" in {
    val random = new Random(32)
    for (_ <- Range(0, 100)) {
      val dataWidth = Vector(8, 16, 32)(random.between(0, 3))
      val len = random.between(1, 1025)
      val stack = new Stack[Long]
      test(new StackModule(
        dataWidth = dataWidth,
        len = len
      )) {
        s =>
          stack.clear()
          s.reset.poke(1.B)
          s.io.in.poke(0.U)
          s.clock.step(1)
          s.io.out.expect(0.U)
          s.io.underflow.expect(0.B)
          s.io.overflow.expect(0.B)
          s.io.isEmpty.expect(stack.isEmpty.B)
          s.io.isFull.expect((stack.length == len).B)
          s.io.popped.expect(0.B)
          s.io.peeked.expect(0.B)
          
          s.reset.poke(0.B)

          for (__ <- Range(0, 100)) {
            val instID = random.between(0, 3)
            instID match {
              case 0 => pushToStack(random, dataWidth, len, stack, s)
              case 1 => popFromStack(random, dataWidth, len, stack, s)
              case 2 => peekAtStack(random, dataWidth, len, stack, s)
            }
          }
      }
    }
  }
}

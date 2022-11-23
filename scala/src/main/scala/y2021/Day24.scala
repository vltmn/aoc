package io.vltmn.aoc
package y2021


import y2021.Day24.{BinaryInstruction, InpInstruction, Instruction, State}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class Day24 extends Solution {
  def parseInput(input: String): Seq[Instruction] = input
    .linesIterator
    .map(l => Instruction(l))
    .toSeq

  def applyInstruction(instruction: Instruction, state: State, inputs: Seq[Int]): (State, Seq[Int]) = instruction match {
    case InpInstruction(op) => (state.withRegisterValue(op, inputs.head), inputs.tail)
    case BinaryInstruction(operator, opA, opB) =>
      val bVal = opB.fold(identity, state.getRegister)
      val aVal = state.getRegister(opA)
      val newVal = operator match {
        case "add" => aVal + bVal
        case "mul" => aVal * bVal
        case "div" => aVal / bVal
        case "mod" => aVal % bVal
        case "eql" => if (aVal == bVal) 1 else 0
      }
      (state.withRegisterValue(opA, newVal), inputs)
  }

  val cache = mutable.Map.empty[(Instruction, State, Option[Int]), State]

  @tailrec
  final def runProgram2(program: Seq[Instruction], state: State, inputs: Seq[Int]): State = program match {
    case instr :: rest =>
      val inputCacheKey = instr match {
        case InpInstruction(_) => Some(inputs.head)
        case _ => None
      }
      val nextState = cache.getOrElseUpdate((instr, state, inputCacheKey), {
        applyInstruction(instr, state, inputs)._1
      })
      val nextInputs = if(inputCacheKey.isDefined) inputs.tail else inputs
      runProgram2(rest, nextState, nextInputs)
    case _ => state
  }

  @tailrec
  final def runProgram(program: Seq[Instruction], state: State, inputs: Seq[Int]): State = program match {
    case instr :: rest =>
      val (nextState, nextInputs) = applyInstruction(instr, state, inputs)
      runProgram(rest, nextState, nextInputs)
    case _ => state
  }

  def findHighestModelNumber(program: Seq[Instruction]): Long = {
    val defaultState = State(0, 0, 0, 0)
    def isValid(value: Long): Boolean = {
      val inputs = value.toString.map(_.asDigit)
      val endState = runProgram2(program, defaultState, inputs)
      endState.z == 0
    }

    @tailrec
    def inner(value: Long): Long = {
      val includes0s = value.toString.contains("0")
      if(!includes0s && isValid(value))
        value
      else
        inner(value - 1)
    }
    inner(99999999999999L)
  }

  def part1(input: String): Long = {
    val program = parseInput(input)
    findHighestModelNumber(program)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    s"Part1: $p1"
  }
}

object Day24 {
  trait Instruction

  case class InpInstruction(op: Char) extends Instruction

  case class BinaryInstruction(operator: String, opA: Char, opB: Either[Int, Char]) extends Instruction

  object Instruction {
    def apply(line: String): Instruction = {
      val splitted = line.split(" ")
      if (splitted.length == 2)
        InpInstruction(splitted.last.toCharArray.head)
      else
        BinaryInstruction(splitted.head, splitted(1).head, Try(splitted.last.toInt).map(i => Left(i)).getOrElse(Right(splitted.last.head)))
    }
  }

  case class State(x: Int, y: Int, z: Int, w: Int) {
    def withRegisterValue(register: Char, value: Int): State = register match {
      case 'x' => copy(x = value)
      case 'y' => copy(y = value)
      case 'z' => copy(z = value)
      case 'w' => copy(w = value)
    }

    def getRegister(register: Char): Int = register match {
      case 'x' => x
      case 'y' => y
      case 'z' => z
      case 'w' => w
    }
  }
}

package day05

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Using


object IntcodeComputer {


  val sep = ","

  def apply(str: String): IntcodeComputer = {
    new IntcodeComputer(str.split(sep).map(_.toInt).toVector)
  }

  def fromFile(filename: String): IntcodeComputer = {
    IntcodeComputer(Using(Source.fromFile(filename)) { source => source.mkString.trim }.get)
  }
}

case class IntcodeComputer(data: Vector[Int], ptr: Int = 0) {

  type Instr = Int
  type Mode = Int

  private def readWriteMap(instr: Instr): String = instr match {
    case 1 => "rrw"
    case 2 => "rrw"
    case 3 => "w"
    case 4 => "r"
    case 5 => "rr"
    case 6 => "rr"
    case 7 => "rrw"
    case 8 => "rrw"
    case _ => ""
  }

  private def numParameters(instr: Instr): Int = readWriteMap(instr).length


  private def fromOpcode(opcode: Int): (Instr, Seq[Mode]) = {
    val instr = opcode.toString.takeRight(2).toInt
    val modes = opcode
      .toString
      .reverse
      .padTo(2 + numParameters(instr), '0')
      .drop(2)
      .map(_.toString.toInt)
    (instr, modes)
  }

  private def getVal(i: Int, mode: Mode): Int = mode match {
    case 0 => data(data(i))
    case 1 => data(i)
  }


  private def getParams(instr: Instr, modes: Seq[Mode]): Seq[Int] = {
    modes.zip(readWriteMap(instr)).zipWithIndex.map {
      case ((m, c), i) => c match {
        case 'r' => getVal(ptr + i + 1, m)
        case 'w' => getVal(ptr + i + 1, 1)
      }
    }

  }

  def step(): IntcodeComputer = {
    val (instr, modes) = fromOpcode(data(ptr))

    val params = getParams(instr, modes)

    instr match {
      case 1 =>
        val Seq(v1, v2, i) = params
        IntcodeComputer(data.updated(i, v1 + v2), ptr + 4)
      case 2 =>
        val Seq(v1, v2, i) = params
        IntcodeComputer(data.updated(i, v1 * v2), ptr + 4)
      case 3 =>
        val Seq(i) = params
        val v = readLine.toInt
        IntcodeComputer(data.updated(i, v), ptr + 2)
      case 4 =>
        val Seq(v) = params
        println(v)
        IntcodeComputer(data, ptr + 2)
      case 5 =>
        val Seq(v, newPtr) = params
        if (v != 0)
          IntcodeComputer(data, newPtr)
        else
          IntcodeComputer(data, ptr + 3)
      case 6 =>
        val Seq(v, newPtr) = params
        if (v == 0)
          IntcodeComputer(data, newPtr)
        else
          IntcodeComputer(data, ptr + 3)
      case 7 =>
        val Seq(v1, v2, i) = params
        IntcodeComputer(data.updated(i, if (v1 < v2) 1 else 0), ptr + 4)
      case 8 =>
        val Seq(v1, v2, i) = params
        IntcodeComputer(data.updated(i, if (v1 == v2) 1 else 0), ptr + 4)
      case op =>
        println(this)
        throw new RuntimeException("Invalid instruction: " + op)
    }
  }

  @tailrec
  final def run(): IntcodeComputer =
    data(ptr) match {
      case 99 => this
      case _ => step().run()
    }

  @tailrec
  final def debug(): IntcodeComputer =
    fromOpcode(data(ptr))._1 match {
      case 99 => this
      case _ =>
        println("ptr = " + this.ptr)
        println("current position = ..." + this.data.slice(this.ptr, this.ptr + 5).mkString(", "))
//        println(this)
        step().debug()
    }
}


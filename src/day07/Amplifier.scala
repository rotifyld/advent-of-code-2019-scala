package day07

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using


object Amplifier {
  def apply(str: String): Amplifier = {
    new Amplifier(str.split(",").map(_.toInt).toVector)
  }

  def fromFile(filename: String): Amplifier = {
    Amplifier(Using(Source.fromFile(filename)) { source => source.mkString.trim }.get)
  }

  def dummy(): Amplifier = new Amplifier(Vector())
}

case class Amplifier(data: Vector[Int],
                     ptr: Int = 0,
                     in: Seq[Int] = Seq(),
                     out: Seq[Int] = Seq()) {
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

  def step(): Amplifier = {
    val (instr, modes) = fromOpcode(data(ptr))
    val params = getParams(instr, modes)

    instr match {
      case 1 =>
        val Seq(v1, v2, i) = params
        this.copy(data.updated(i, v1 + v2), ptr + 4)
      case 2 =>
        val Seq(v1, v2, i) = params
        this.copy(data.updated(i, v1 * v2), ptr + 4)
      case 3 =>
        val Seq(i) = params
        val v = in.head
        this.copy(data.updated(i, v), ptr + 2, in = in.tail)
      case 4 =>
        val Seq(v) = params
        this.copy(data, ptr + 2, out = out :+ v)
      case 5 =>
        val Seq(v, newPtr) = params
        if (v != 0)
          this.copy(data, newPtr)
        else
          this.copy(data, ptr + 3)
      case 6 =>
        val Seq(v, newPtr) = params
        if (v == 0)
          this.copy(data, newPtr)
        else
          this.copy(data, ptr + 3)
      case 7 =>
        val Seq(v1, v2, i) = params
        this.copy(data.updated(i, if (v1 < v2) 1 else 0), ptr + 4)
      case 8 =>
        val Seq(v1, v2, i) = params
        this.copy(data.updated(i, if (v1 == v2) 1 else 0), ptr + 4)
      case op =>
        println(this)
        throw new RuntimeException("Invalid instruction: " + op)
    }
  }


  @tailrec
  final def runUntilOut(): (Amplifier, Seq[Int]) = {
    if (out.isEmpty) {
      data(ptr) match {
        case 99 => (this, Seq())
        case _ => step().runUntilOut()
      }
    } else {
      (this.copy(out = Seq()), out)
    }
  }


  @tailrec
  final def run(): Seq[Int] = {
    data(ptr) match {
      case 99 => out
      case _ => step().run()
    }
  }

  @tailrec
  final def debug(): Seq[Int] = {
    data(ptr) match {
      case 99 =>
        println(out)
        out
      case _ =>
        println("ptr = " + this.ptr)
        println("current position = ..." + this.data.slice(this.ptr, this.ptr + 5).mkString(", "))
        step().debug()
    }
  }

  def put(in: Int): Amplifier = {
    this.copy(in = this.in :+ in)
  }

}


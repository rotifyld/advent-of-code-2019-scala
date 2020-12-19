package day11

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using


object IntcodeComputer {
  def apply(str: String): IntcodeComputer = {
    new IntcodeComputer(str.split(",").map(_.toLong).toVector.padTo(10000, 0L))
  }

  def fromFile(filename: String = "data/%s/input".format(this.getClass.getPackage.getName)): IntcodeComputer = {
    IntcodeComputer(Using(Source.fromFile(filename)) { source => source.mkString.trim }.get)
  }

  def dummy(): IntcodeComputer = new IntcodeComputer(Vector())
}


case class IntcodeComputer(data: Vector[Long],
                           ptr: Int = 0,
                           relBase: Int = 0,
                           in: Seq[Long] = Seq(),
                           out: Seq[Long] = Seq()) {
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
    case 9 => "r"
    case _ => ""
  }

  private def numParameters(instr: Instr): Int = readWriteMap(instr).length

  private def fromOpcode(opcode: Long): (Instr, Seq[Mode]) = {
    val instr = opcode.toString.takeRight(2).toInt
    val modes = opcode
      .toString
      .reverse
      .padTo(2 + numParameters(instr), '0')
      .drop(2)
      .map(_.toString.toInt)
    (instr, modes)
  }

  private def currentOpcode(): (Instr, Seq[Mode]) = fromOpcode(data(ptr))

  private def getParams(rws: String, modes: Seq[Mode]): Seq[Long] = {
    assert(rws.length == modes.length)
    (rws zip modes).zipWithIndex.map {
      case (('r', 0), i) => data(data(ptr + i + 1).toInt)
      case (('r', 1), i) => data(ptr + i + 1)
      case (('r', 2), i) => data(relBase + data(ptr + i + 1).toInt)
      case (('w', 0), i) => data(ptr + i + 1)
      case (('w', 2), i) => relBase + data(ptr + i + 1)
      case ((rw, m), _) => assert(false, "Invalid (rw, m): (%c, %d)".format(rw, m)); 0L
    }
  }

  def step(): IntcodeComputer = {
    val (instr, modes) = currentOpcode()
    val params = getParams(readWriteMap(instr), modes)

    instr match {
      case 1 =>
        val Seq(v1, v2, i) = params
        this.copy(data.updated(i.toInt, v1 + v2), ptr + 4)
      case 2 =>
        val Seq(v1, v2, i) = params
        this.copy(data.updated(i.toInt, v1 * v2), ptr + 4)
      case 3 =>
        val Seq(i) = params
        val v = in.head
        this.copy(data.updated(i.toInt, v), ptr + 2, in = in.tail)
      case 4 =>
        val Seq(v) = params
        this.copy(ptr = ptr + 2, out = out :+ v)
      case 5 =>
        val Seq(v, newPtr) = params
        if (v != 0)
          this.copy(ptr = newPtr.toInt)
        else
          this.copy(ptr = ptr + 3)
      case 6 =>
        val Seq(v, newPtr) = params
        if (v == 0)
          this.copy(ptr = newPtr.toInt)
        else
          this.copy(ptr = ptr + 3)
      case 7 =>
        val Seq(v1, v2, i) = params
        this.copy(data.updated(i.toInt, if (v1 < v2) 1 else 0), ptr + 4)
      case 8 =>
        val Seq(v1, v2, i) = params
        this.copy(data.updated(i.toInt, if (v1 == v2) 1 else 0), ptr + 4)
      case 9 =>
        val Seq(v) = params
        this.copy(ptr = ptr + 2, relBase = (relBase + v).toInt)
      case op =>
        println(this)
        throw new RuntimeException("Invalid instruction: " + op)
    }
  }

  @tailrec
  final def run(): IntcodeComputer = {
    currentOpcode()._1 match {
      case 99 => this
      case 3 if in.isEmpty => this
      case _ => step().run()
    }
  }

  def terminated(): Boolean = currentOpcode()._1 == 99

  def push(ins: Int*): IntcodeComputer = this.copy(in = in ++ ins.map(_.toLong))

  def pop(): (IntcodeComputer, Seq[Long]) = (this.copy(out = Seq()), out)
}


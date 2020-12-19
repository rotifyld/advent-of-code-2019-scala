package day11

import scala.annotation.tailrec


object Main11 extends App {

  implicit class TuppleAdd(t: (Int, Int)) {
    def +(p: (Int, Int)): (Int, Int) = (p._1 + t._1, p._2 + t._2)
  }

  type Coords = (Int, Int)

  @tailrec
  def go(emergencyHullPaintingRobot: IntcodeComputer,
         position: Coords = (0, 0),
         dir: Dir = Up,
         painted: Set[Coords] = Set(),
         whites: Set[Coords] = Set()
        ): (Int, Set[(Int, Int)]) = {
    val color = if (whites contains position) 1 else 0
    emergencyHullPaintingRobot.push(color).run().pop() match {
      case (_, Seq()) => (painted.size, whites)
      case (robot, Seq(color, turn)) =>
        val newDir = if (turn == 1) dir.right() else dir.left()
        go(robot,
          position + newDir.asCoords(),
          newDir,
          painted + position,
          if (color == 1) whites + position else whites - position)
    }
  }

  println("task one", go(IntcodeComputer.fromFile())._1)

  println("task two", {
    val whites = go(IntcodeComputer.fromFile(), whites = Set((0, 0)))._2
    val (yMin, yMax) = {
      val ys = whites.map(_._2)
      (ys.min - 1, ys.max + 1)
    }
    val (xMin, xMax) = {
      val xs = whites.map(_._1)
      (xs.min - 1, xs.max + 1)
    }
    val size = whites.flatMap(t => Seq(t._1, t._2)).map(_.abs).max + 1
    (yMin to yMax).map { y =>
      (xMin to xMax).map { x =>
        if (whites contains(x, y)) ' ' else '█'
      }.mkString
    }.reverse.mkString("\n")
  })
}

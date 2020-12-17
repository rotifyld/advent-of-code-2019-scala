package day03

import scala.io.Source
import scala.util.Using

object Main03 extends App {

  type Coords = (Int, Int)
  type Segment = (Coords, Coords)
  type Wire = Seq[Segment]

  def newWire(str: String): Wire = {
    val intermediateCoords = str
      .split(",")
      .map((trace: String) => {
        val len = trace.tail.toInt
        trace.head match {
          case 'U' => (0, len)
          case 'D' => (0, -len)
          case 'L' => (-len, 0)
          case 'R' => (len, 0)
        }
      })
      .scanLeft((0, 0)) {
        case ((x, y), (x_, y_)) => (x + x_, y + y_)
      }

    intermediateCoords zip intermediateCoords.tail
  }

  def sortWire(wire: Wire): Wire = {
    wire.map {
      case (t1, t2) if (t1._1 > t2._1 || t1._2 > t2._2) => (t2, t1)
      case c => c
    }
  }

  def intersections(s1: Segment, s2: Segment): Seq[Coords] = (s1, s2) match {
    case (((x1from, y1from), (x1to, y1to)), ((x2from, y2from), (x2to, y2to)))
      if (x1from == x1to && y2from == y2to) && (y1from < y2from && y2to < y1to && x2from < x1from && x1to < x2to)
    => Seq((x1from, y2from))
    case (((x1from, y1from), (x1to, y1to)), ((x2from, y2from), (x2to, y2to)))
      if (y1from == y1to && x2from == x2to) && (x1from < x2from && x2to < x1to && y2from < y1from && y1to < y2to)
    => Seq((x2from, y1from))
    case _ => Seq()
  }

  def wireDistance(wire: Wire, intersection: Coords): Int = wire.head match {
    case ((x, y), (x_, y_)) => intersection match {
      case (xi, yi) =>
        if (((x <= xi && xi <= x_) || (x_ <= xi && xi <= x)) && ((y <= yi && yi <= y_) || (y_ <= yi && yi <= y))) {
          (xi - x).abs + (yi - y).abs
        } else
          (x_ - x).abs + (y_ - y).abs + wireDistance(wire.tail, intersection)
    }
  }

  val filename = "data/day03/input"

  val lines = Using(Source.fromFile(filename)) { source => source.mkString }.get
  val Array(wire1, wire2) = lines.split("\n").map(newWire)
  val isections = for {
    s1 <- sortWire(wire1)
    s2 <- sortWire(wire2)
    isection <- intersections(s1, s2)
  } yield isection
  println("Part1 = " + isections.map({ case (x, y) => x.abs + y.abs }).min)
  println("Part2 = " + isections.map((i: Coords) => wireDistance(wire2, i) + wireDistance(wire1, i)).min)
}

package day15

import day15.Screen.{EMPTY, NONE, OXYGEN, charMap}
import utils.{Down, Left, Right, Up}
import utils.Utils.Tupple2Add


object Screen {
  val WALL = 0
  val EMPTY = 1
  val OXYGEN = 2
  val DROID = 3
  val NONE = 4

  def charMap: Int => Char = {
    case WALL => '█'
    case EMPTY => '.'
    case OXYGEN => 'O'
    case DROID => 'D'
    case NONE => ' '
  }

  def apply() = new Screen(Map((0, 0) -> DROID))
}

case class Screen(screen: Map[(Int, Int), Int]) {

  def draw(): Unit = {
    val (yMin, yMax) = {
      val ys = screen.keys.map(_._2)
      (ys.min, ys.max)
    }
    val (xMin, xMax) = {
      val xs = screen.keys.map(_._1)
      (xs.min, xs.max)
    }

    (yMin to yMax).foreach { y =>
      (xMin to xMax).foreach { x =>
        print(charMap(screen.getOrElse((x, y), NONE)))
      }
      println()
    }
  }

  def updated(k: (Int, Int), v: Int): Screen = this.copy(screen.updated(k, v))

  def explored: Boolean = this.screen.forall {
    case (coords, EMPTY) => Seq(Up, Down, Left, Right).forall(d => this.screen.contains(coords + d.toCoords))
    case _ => true
  }

  def evolve: Option[Screen] = {
    val newOxygens = screen.filter {
      case (coords, EMPTY) =>
        Seq(Up, Down, Left, Right).exists(d => this.screen(coords + d.toCoords) == OXYGEN)
      case _ => false
    }.keys

    if (newOxygens.isEmpty)
      None
    else
      Some(this.copy(newOxygens.foldLeft(screen) { case (screen, coords) => screen.updated(coords, OXYGEN) }))
  }
}

package day15

import day15.Screen.{DROID, EMPTY, OXYGEN, WALL}
import utils.{Dir, Down, Left, Right, Up}
import utils.Utils.{Tupple2Add, time}

import scala.annotation.tailrec
import scala.util.Random

object Main15 extends App {

  @tailrec
  final def loop(pos: (Int, Int), screen: Screen, robot: IntcodeComputer, dist: Map[(Int, Int), Int]): Int = {
    val dir = Random.nextInt(4) + 1
    val (newRobot, Seq(out)) = robot.runWith(dir)
    val newPos = pos + Dir.fromNumber(dir).toCoords
    out match {
      case OXYGEN => screen.updated((0, 0), DROID).updated(pos, OXYGEN).draw(); dist(pos) + 1
      case WALL => loop(pos, screen.updated(newPos, WALL), newRobot, dist)
      case EMPTY =>
        loop(newPos, screen.updated(pos, EMPTY).updated(newPos, DROID), newRobot, dist.updatedWith(newPos) {
          case Some(i) => Some(i)
          case None => Some(dist(pos) + 1)
        })
    }
  }

  @tailrec
  final def loop2(pos: (Int, Int), screen: Screen, robot: IntcodeComputer): Screen = {
    if (screen.explored)
      screen
    else {
      val unknownDirs = Seq(Up, Down, Left, Right).filterNot(d => screen.screen.contains(d.toCoords + pos))
      val dir =
        if (unknownDirs.nonEmpty)
          unknownDirs.head.toInt
        else
          Random.nextInt(4) + 1
      val (newRobot, Seq(out)) = robot.runWith(dir)
      val newPos = pos + Dir.fromNumber(dir).toCoords
      out match {
        case WALL => loop2(pos, screen.updated(newPos, WALL), newRobot)
        case EMPTY => loop2(newPos, screen.updated(newPos, EMPTY), newRobot)
        case OXYGEN => loop2(newPos, screen.updated(newPos, OXYGEN), newRobot)
      }
    }
  }

  @tailrec
  final def loop3(screen: Screen, i: Int): Int = {
    screen.evolve match {
      case None => i
      case Some(screen) => loop3(screen, i + 1)
    }
  }

  //  println(loop((0, 0), Screen(), IntcodeComputer.fromFile(), Map((0, 0) -> 0)))
  val finalScreen = time {
    loop2((0, 0), new Screen(Map((0, 0) -> EMPTY)), IntcodeComputer.fromFile())
  }
  finalScreen.draw()
  finalScreen.evolve.get.draw()

  println(loop3(finalScreen, 0))



}

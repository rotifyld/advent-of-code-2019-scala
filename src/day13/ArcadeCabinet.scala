package day13

import scala.annotation.tailrec
import scala.io.StdIn.readLine


case class ArcadeCabinet(brain: IntcodeComputer, screen: Map[(Int, Int), Int] = Map(), score: Int = 0) {

  val SCORE_COORDS: (Int, Int) = (-1, 0)

  def run(): ArcadeCabinet = {
    val (newBrain, out) = brain.run().pop()
    val newScreen = out.sliding(3, 3).foldLeft(screen) {
      case (screen, Seq(x, y, 0)) => screen.removed((x.toInt, y.toInt))
      case (screen, Seq(x, y, id)) => screen.updated((x.toInt, y.toInt), id.toInt)
    }
    this.copy(newBrain, newScreen.removed(SCORE_COORDS), newScreen.getOrElse(SCORE_COORDS, score))
  }

  def printScreen(): Unit = {
    val (yMin, yMax) = {
      val ys = screen.keys.map(_._2)
      (ys.min, ys.max)
    }
    val (xMin, xMax) = {
      val xs = screen.keys.map(_._1)
      (xs.min, xs.max)
    }

    println("Score = %8d pts".format(score))
    (yMin to yMax).foreach { y =>
      (xMin to xMax).foreach { x =>
        print(screen.getOrElse((x, y), 0) match {
          case 0 => ' '
          case 1 => '█'
          case 2 => '▒'
          case 3 => '━'
          case 4 => 'O'
        })
      }
      println()
    }
  }

  @tailrec
  final def play(): Int = {
    if (brain.terminated())
      score
    else {
      printScreen()
      val xPaddle = screen.find(_._2 == 3).get._1._1
      val xBall = screen.find(_._2 == 4).get._1._1

      val in = if (xPaddle < xBall) 1 else if (xPaddle > xBall) -1 else 0
      this.copy(brain = brain.push(in)).run().play()
    }
  }
}

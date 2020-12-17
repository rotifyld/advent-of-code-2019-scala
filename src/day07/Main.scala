package day07

import scala.annotation.tailrec

object Main extends App {

  val amp = Amplifier.fromFile("data/day07/input")

  val taskOne = (0 to 4)
    .permutations
    .map {
      phases =>
        phases.foldLeft(0) {
          case (prevOut, phase) => amp.put(phase).put(prevOut).runUntilOut()._2.head
        }
    }.max

  @tailrec
  def taskTwo(amps: Seq[Amplifier], prevOut: Int): Int = {

    if (amps.head.put(prevOut).runUntilOut()._2.isEmpty)
      prevOut
    else {
      val scan = amps.scanLeft((Amplifier.dummy(), Seq(prevOut))) {
        case ((_, Seq(prevOut)), amp) => amp.put(prevOut).runUntilOut().copy()
      }.tail

      val ret = scan.last._2
      val newAmps = scan.map(_._1)

      taskTwo(newAmps, ret.head)
    }
  }


  //  println(taskOne)

  println((5 to 9)
    .permutations
    .map { phases =>
      val amps = phases.map(amp.put)
      taskTwo(amps, 0)
    }.max)

  //  println(taskTwo(amps, 0))

}

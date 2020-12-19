package day09

object Main09 extends App {

  val comp = IntcodeComputer.fromFile("data/day09/input")

  println(comp.put(1).run())

  println(comp.put(2).run())

}

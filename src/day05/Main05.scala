package day05

object Main05 extends App {
  val computer = IntcodeComputer.fromFile("data/day05/input")
  computer.run()
}

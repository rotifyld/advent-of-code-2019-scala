package day13

object Main13 extends App {

  val arcade = ArcadeCabinet(IntcodeComputer.fromFile())

  println("task one", {
    val finalScreen = arcade.run().screen
    finalScreen.count(_._2 == 2)
  })

  println("task two", arcade.copy(brain = arcade.brain.copy(data = arcade.brain.data.updated(0, 2))).run().play())

}

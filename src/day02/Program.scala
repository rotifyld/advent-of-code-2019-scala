package day02

case class Program(data: Vector[Int], ptr: Int = 0) {
  def step(): Program =

    data(ptr) match {
      case 1 =>
        val i = data(ptr + 3)
        val v = data(data(ptr + 1)) + data(data(ptr + 2))
        Program(data.updated(i, v), ptr + 4)
      case 2 =>
        val i = data(ptr + 3)
        val v = data(data(ptr + 1)) * data(data(ptr + 2))
        Program(data.updated(i, v), ptr + 4)
      case _ => this
    }

  def run(): Program =

    data(ptr) match {
      case 99 => this
      case _ => step().run()
    }
}

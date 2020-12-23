package utils

object Utils {
  // adapted from http://biercoff.com/easily-measuring-code-execution-time-in-scala/
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    val dt = t1 - t0
    println("Elapsed time: %d,%3d s".format(dt / 1000, dt % 1000))
    result
  }

  // https://stackoverflow.com/a/19606951
  implicit class Tupple2Add[A: Numeric, B: Numeric](t: (A, B)) {

    import Numeric.Implicits._

    def +(p: (A, B)): (A, B) = (p._1 + t._1, p._2 + t._2)
  }

  def ceilDiv(x: Int, y: Int): Int = {
    (x + y - 1) / y
  }

}

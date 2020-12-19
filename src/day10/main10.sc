import scala.collection.immutable.Range

implicit class TuppleAdd(t: (Int, Int)) {
  def +(p: (Int, Int)): (Int, Int) = (p._1 + t._1, p._2 + t._2)

  def -(p: (Int, Int)): (Int, Int) = (t._1 - p._1, t._2 - p._2)
}

val inn =
  """.##.#.#....#.#.#..##..#.#.
    |#.##.#..#.####.##....##.#.
    |###.##.##.#.#...#..###....
    |####.##..###.#.#...####..#
    |..#####..#.#.#..#######..#
    |.###..##..###.####.#######
    |.##..##.###..##.##.....###
    |#..#..###..##.#...#..####.
    |....#.#...##.##....#.#..##
    |..#.#.###.####..##.###.#.#
    |.#..##.#####.##.####..#.#.
    |#..##.#.#.###.#..##.##....
    |#.#.##.#.##.##......###.#.
    |#####...###.####..#.##....
    |.#####.#.#..#.##.#.#...###
    |.#..#.##.#.#.##.#....###.#
    |.......###.#....##.....###
    |#..#####.#..#..##..##.#.##
    |##.#.###..######.###..#..#
    |#.#....####.##.###....####
    |..#.#.#.########.....#.#.#
    |.##.#.#..#...###.####..##.
    |##...###....#.##.##..#....
    |..##.##.##.#######..#...#.
    |.###..#.#..#...###..###.#.
    |#..#..#######..#.#..#..#.#
    |""".stripMargin

val in =
  """.#..##.###...#######
    |##.############..##.
    |.#.######.########.#
    |.###.#######.####.#.
    |#####.##.#.##.###.##
    |..#####..#.#########
    |####################
    |#.####....###.#.#.##
    |##.#################
    |#####.##.###..####..
    |..######..##.#######
    |####.##.####...##..#
    |.#####..#.######.###
    |##...#.##########...
    |#.##########.#######
    |.####.#.###.###.#.##
    |....##.##.###..#####
    |.#.#.###########.###
    |#.#.#.#####.####.###
    |###.##.####.##.#..##""".stripMargin

val ii =
  """.#..#
    |.....
    |#####
    |....#
    |...##
    |""".stripMargin

val ass = inn.split("\n").map(_.map(_ == '#').toArray)

val asteroids = for {
  (as, j) <- ass.zipWithIndex
  (a, i) <- as.zipWithIndex
  if a
} yield (i, j)

asteroids.length

val taskOne = asteroids.map { a =>
  val dists: Set[(Int, Int)] = asteroids.map(_ - a).toSet - ((0, 0))
  //  println(a1, dists)

  val valid = dists.filter {
    case (0, n) => !(1 until n.abs).exists(i => dists.contains(0, n.sign * i))
    case (n, 0) => !(1 until n.abs).exists(i => dists.contains(n.sign * i, 0))
    case (x, y) =>
      val gcd = BigInt(x).gcd(y).toInt
      !(1 until gcd).exists(i => dists.contains(x / gcd * i, y / gcd * i))
  }
  //  println(valid.size, valid)

  a -> valid.size
}

val solution = taskOne(taskOne.indexWhere(_._2 == taskOne.map(_._2).max))

val a0 = solution._1
val distsFrom = asteroids.map(_ - a0).filterNot(_ == (0, 0))

val byAngle = distsFrom.map {
  case (x, y) => (math.atan2(-y, x), if (x * y == 0) x.abs + y.abs else BigInt(x).gcd(y).toInt, x, y)
}

val anglesOrder = {
  val angles = byAngle.map(_._1).distinct.sorted.reverse
  println(angles.mkString("Array(", ", ", ")"))
  val (s1, s2) = angles.splitAt(angles.indexWhere(_ <= (math.Pi / 2)))
  println(s1.mkString("Array(", ", ", ")"))
  println(s2.mkString("Array(", ", ", ")"))
  s2 ++ s1
}

val groups = byAngle.groupBy(_._1).map { case (k, v) => (k, v.map { case (a, b, c, d) => (b, c, d) }.sorted) }
val groupsLen = groups.size
val maxGroupSize = groups.toSeq.map(_._2.length).max

val vaporOrd = for {
  i <- 0 until maxGroupSize
  a <- anglesOrder
  arr = groups(a)
  if i < arr.length
} yield arr(i)

vaporOrd(199) match {
  case (_, dx, dy) => a0 + (dx, dy)
}


//def go(i: Int, groups: Map[Double, Seq[(Int, Int, Int)]], angles: Seq[Double]): (Int, Int) = {
//
//}

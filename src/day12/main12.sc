import scala.annotation.tailrec

implicit class TuppleAdd(t: (Int, Int, Int)) {
  def +(p: (Int, Int, Int)): (Int, Int, Int) = (p._1 + t._1, p._2 + t._2, p._3 + t._3)

  def -(p: (Int, Int, Int)): (Int, Int, Int) = (t._1 - p._1, t._2 - p._2, t._3 - p._3)

  def <(p: (Int, Int, Int)): (Boolean, Boolean, Boolean) = (t._1 < p._1, t._2 < p._2, t._3 < p._3)

  def >(p: (Int, Int, Int)): (Boolean, Boolean, Boolean) = p < t
}

implicit def bool2int(b: Boolean): Int = if (b) 1 else 0
implicit def t3bool2t3int(b: (Boolean, Boolean, Boolean)): (Int, Int, Int) = (b._1, b._2, b._3)

val in =
  """<x=1, y=-4, z=3>
    |<x=-14, y=9, z=-4>
    |<x=-4, y=-6, z=7>
    |<x=6, y=-9, z=-11>
    |""".stripMargin

val inTest =
  """<x=-1, y=0, z=2>
    |<x=2, y=-10, z=-7>
    |<x=4, y=-8, z=8>
    |<x=3, y=5, z=-1>""".stripMargin

type Coords = (Int, Int, Int)

val pattern = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r

val initMoons = pattern.findAllMatchIn(in).map { m =>
  val coords = (1 to 3).map(m.group).map(_.toInt)
  ((coords(0), coords(1), coords(2)), (0, 0, 0))
}.toSeq

def update(moons: Seq[(Coords, Coords)]): Seq[(Coords, Coords)] = {
  val newVs = moons.map { case (pos, vs) =>
    moons.map { case (pos2, _) =>
      (0, 0, 0) + (pos < pos2) - (pos > pos2)
    }.fold(vs)(_ + _)
  }
  moons.zip(newVs).map { case ((pos, _), newVs) =>
    (pos + newVs, newVs)
  }
}

val finalMoons = (1 to 1000).foldRight(initMoons) { case (_, moons) =>
  update(moons)
}

finalMoons.map { case ((x, y, z), (vx, vy, vz)) =>
  (x.abs + y.abs + z.abs) * (vx.abs + vy.abs + vz.abs)
}.sum

@tailrec
def go(moons: Seq[((Int, Int, Int), (Int, Int, Int))], i: Int): Any = {
  if (moons == initMoons)
    i
  else {
    println(i)
    go(update(moons), i + 1)
  }
}

go(update(initMoons), 1)
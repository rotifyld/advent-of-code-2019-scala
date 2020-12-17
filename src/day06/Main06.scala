package day06

import com.sun.xml.internal.ws.client.sei.ResponseBuilder.Body

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Main06 extends App {
  val in = Using(Source.fromFile("data/day06/input")) { source => source.mkString.trim }.get

  val parsed = for {
    line <- in.split("\n")
    Array(orbited, orbiting) = line.split("\\)")
  } yield (orbited, orbiting)

  val orbitGraph = parsed.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
  val reversedOrbitGraph = parsed.map(t => (t._2, t._1)).toMap

  def taskOne(body: String): (Int, Int) = {
    orbitGraph.get(body) match {
      case None => (0, 0)
      case Some(children) =>
        val rets = children.map(taskOne)
        val (childrenSOlution, childrenAncestors) = rets.fold((0, 0)) { case ((a1, b1), (a2, b2)) => (a1 + a2, b1 + b2) }
        val ancestors = childrenAncestors + children.length
        (childrenSOlution + ancestors, ancestors)
    }
  }

  def taskTwo(): Int = {
    def pathFrom(body: String): Seq[String] = {
      if (body == "COM")
        Seq("COM")
      else
        body +: pathFrom(reversedOrbitGraph(body))
    }

    val fromYou = pathFrom("YOU")
    val fromSan = pathFrom("SAN")

    val numCommonOrbits = fromYou.reverse.zip(fromSan.reverse).count { case (s1, s2) => s1 == s2 }
    fromSan.length + fromYou.length - (2 * numCommonOrbits) + 1 - 2 - 1
  }

  println(taskOne("COM"))
  println(taskTwo())
}

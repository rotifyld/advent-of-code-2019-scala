package day08

import scala.io.Source
import scala.util.Using

object Main08 extends App {

  val in = Using(Source.fromFile("data/day08/input")) { src => src.mkString.trim }.get

  val width = 25
  val height = 6
  val layerSize = width * height

  val taskOne = {
    val min0Idx = in.grouped(layerSize).map(_.count(_ == '0')).zipWithIndex.min._2
    val max0Layer = in.slice(layerSize * min0Idx, layerSize * min0Idx + layerSize)
    max0Layer.count(_ == '1') * max0Layer.count(_ == '2')
  }

  val taskTwo = {
    val stacked = in.grouped(layerSize).foldLeft("2" * layerSize) {
      case (strOver, strUnder) => strOver.zip(strUnder).map {
        case ('2', c) => c
        case (c, _) => c
      }.mkString
    }

    stacked.map {
      case '0' => ' '
      case '1' => 'X'
    }.grouped(width).mkString("\n")

  }

  println(taskOne)
  println(taskTwo)
}

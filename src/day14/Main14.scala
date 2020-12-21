package day14

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Main14 extends App {

  type Element = String

  val in = Using(Source.fromFile("data/day14/input")) { source => source.mkString }.get

  val recipes: Map[Element, (Int, Seq[(Int, Element)])] = {
    val recipePattern = """(.*)=>(.*)\n""".r
    val ingredientPattern = """(\d+) (\w+)""".r

    recipePattern.findAllMatchIn(in).map { m =>
      val List(ingredientsRaw, productRaw) = m.subgroups
      val (productAmt, productElem) = {
        val List(amt, elem) = ingredientPattern.findFirstMatchIn(productRaw).get.subgroups
        (amt.toInt, elem)
      }
      val ingredients = {
        ingredientPattern.findAllMatchIn(ingredientsRaw).map { m =>
          val List(amt, elem) = m.subgroups
          (amt.toInt, elem)
        }
      }.toSeq
      (productElem, (productAmt, ingredients))
    }.toMap
  }

  val reqs: Map[Element, Int] = {
    val reqSeq = for {
      (p, (_, is)) <- recipes.toSeq
      i <- is.map(_._2)
    } yield (i, p)
    reqSeq.groupBy(_._1).map { case (k, v) => (k, v.size) }
  }.updated("FUEL", 0)

  @tailrec
  final def loop(recipe: Map[Element, Long], reqs: Map[Element, Int]): Long = {
    if (recipe.size == 1 && recipe.contains("ORE"))
      recipe("ORE")
    else {
      val (elem, amt) = recipe.find {
        case (elem, _) => reqs(elem) == 0
      }.get

      val (batchSize, ingredientsBatches) = recipes(elem)
      val ingredients = {
        val multiplier = (amt / batchSize) + (if (amt % batchSize == 0) 0 else 1)
        ingredientsBatches.map { case (i, e) => (i * multiplier, e) }
      }

      val newRecipe = ingredients.foldLeft(recipe.removed(elem)) {
        case (rec, (amt, elem)) => rec.updatedWith(elem) {
          case Some(v) => Some(v + amt)
          case None => Some(amt)
        }
      }

      val newReqs = reqs.map {
        case (elem, amt) if ingredients.map(_._2).contains(elem) => (elem, amt - 1)
        case p => p
      }

      loop(newRecipe, newReqs)
    }
  }

  val taskOne = loop(Map("FUEL" -> 1), reqs)

  println(taskOne)


  val solutionView = (0 to 1000000000).view.map(i => loop(Map("FUEL" -> i.toLong), reqs))
  val taskTwo = solutionView.search(1000000000000L).insertionPoint

  println(taskTwo - 1, solutionView(taskTwo - 1))
  println(taskTwo, solutionView(taskTwo))
  println(taskTwo + 1, solutionView(taskTwo + 1))

}

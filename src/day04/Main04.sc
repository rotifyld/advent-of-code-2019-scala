val in = "156218-652527"
val Array(lo, hi) = in.split("-").map(_.toInt)

val validPart1 = for {
  _1 <- 0 to 9
  _2 <- 0 to _1
  _3 <- 0 to _2
  _4 <- 0 to _3
  _5 <- 0 to _4
  _6 <- 0 to _5
  numArr = Array(_1, _2, _3, _4, _5, _6).reverse
  if (numArr zip numArr.tail).exists { case (i, j) => i == j }
  num = numArr.mkString.toInt
  if lo <= num && num <= hi
} yield num

validPart1.length

val validPart2 = for {
  _1 <- 0 to 9
  _2 <- 0 to _1
  _3 <- 0 to _2
  _4 <- 0 to _3
  _5 <- 0 to _4
  _6 <- 0 to _5
  arr = Array(_1, _2, _3, _4, _5, _6).reverse
  if arr(0) == arr(1) && arr(1) != arr(2) ||
    arr(3) != arr(4) && arr(4) == arr(5) ||
    arr.sliding(4).exists(sl => sl(0) != sl(1) && sl(1) == sl(2) && sl(2) != sl(3))
  num = arr.mkString.toInt
  if lo <= num && num <= hi
} yield num

validPart2.length


package calc

object Calc11 extends App {

  val weights = Seq(16, 16, 4, 4, 1, 1)

  @scala.annotation.tailrec
  def subset(a: Seq[Int], b: Seq[Int]): Seq[Int] = {
    if (b.isEmpty) a
    else {
      a.indexOf(b.head) match {
        case -1 => subset(a, b.tail)
        case n => subset(a.take(n) ++ a.drop(n + 1), b.tail)
      }
    }
  }

  val result = (for {
    n <- 0 to weights.size
    combsLeft <- weights.combinations(n)
    remaining = subset(weights, combsLeft)
    m <- 0 to remaining.size
    combsRight <- remaining.combinations(m) if combsLeft.sum != combsRight.sum
  } yield {
    println(s"left: $combsLeft right: $combsRight delta: ${math.abs(combsLeft.sum - combsRight.sum)}")
    math.abs(combsLeft.sum - combsRight.sum)
  }).distinct.sorted
  println(result)
  println(s"count = ${result.size}")
}

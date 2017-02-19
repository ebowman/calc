package calc

/**
  * Let N denote the concatenation of the first 60 positive integers:
  *   1234567891011121314...585960
  * Remove any 100 digits from N without rearranging the remaining digits, and call the resulting number M.
  * What is the largest possible value of M?
  */
object Calc13 extends App {

  val str = (1 to 60).map(_.toString).reduce(_ + _)
  val map: Map[Int, Int] = (for {
    n <- '0' to '9'
  } yield (n - '0') -> str.count(_ == n)).toMap
  println(str)
  println(str.length)
  println(map)
  // now we can see that there are 6 9's
  // we want to keep as many 9s at the left as possible
  val nineSpots = str.indices.flatMap { i =>
    if (str(i) == '9') Some(i)
    else None
  }
  println(nineSpots)
  // there is a 9 at 8, 28, 48, 68, 88 and 108
  // the one at 108 doesn't leave enough left to make an 11 char string
  // so we remove the first 88 - 5 characters, and end up with 99999
  // so now we have to come up with 6 more from the remainder
  val remainder = str.drop(89)
  println(remainder)

  val leftToDrop = 100 - 89 + 5
  println(leftToDrop)

  val combs = remainder.indices.combinations(leftToDrop).size
  println("combs.size = " + combs)

  @scala.annotation.tailrec
  def strip(str: String, indices: Seq[Int]): Long = {
    if (indices.isEmpty) str.toLong
    else strip(str.take(indices.head) + str.drop(indices.head + 1), indices.tail)
  }

  val all = for {
    comb: Seq[Int] <- remainder.indices.combinations(leftToDrop)
    combSorted = comb.sorted.reverse
  } yield strip(remainder, combSorted)

  val m = all.toSeq.max
  println(m)
  println("99999" + m)
}

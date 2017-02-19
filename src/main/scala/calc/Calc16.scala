package calc

/**
  * If each letter in the expression (a + b/c)(d + e)
  * is replaced by a different digit from 1 through 9, inclusive,
  * what is the smallest possible integer value of the expression?
  */
object Calc16 extends App {

  val results: Iterator[(Double, String)] = for {
    perm <- (1 to 9).permutations
  } yield (perm(0) + perm(1).toDouble/perm(2))*(perm(3) + perm(4)) -> s"(${perm(0)} + ${perm(1)}/${perm(2)})(${perm(3)} + ${perm(4)})"

  val sorted = results.toSeq.filter(x => math.abs(x._1 - math.round(x._1)) < 0.001).sortBy(_._1)
  sorted.take(20).foreach(println)
}

package calc

/**
  * Created by ebowman on 18/02/2017.
  */
object Calc8 extends App {

  def p(n: Int): Int = {
    if (n < 10) n
    else n.toString.replaceAll("0", "").map(_.toString.toInt).product
  }

  def primeFactors(number: Int, list: List[Int] = List()): List[Int] = {
    for(n <- 2 to number if number % n == 0) {
      return primeFactors(number / n, list :+ n)
    }
    list
  }

  val a = 10.toString.replaceAll("0", "")
  val b = a.map(_.toString.toInt)
  println(a)
  println(b)

  assert(p(3) == 3)
  assert(p(9) == 9)
  assert(p(10) == 1)
  assert(p(11) == 1)
  assert(p(23) == 6)
  println(p(3))
  (1 to 10).foreach(x => println(primeFactors(x)))
  println((1 to 999).map(p).sum)
  println(primeFactors((1 to 999).map(p).sum))

  println(primeFactors((1 to 999).map(p).sum).max)
}

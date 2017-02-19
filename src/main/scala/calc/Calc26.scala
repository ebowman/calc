package calc

/**
  * During a meeting from their school mathematics club, Andy, Becky, Chloe and Danny discover that their
  * favourite number (a positive integer) is the same. So, each of the four friends makes three statements
  * about the number, at least one of which is true and at least one of which is false.
  * Andy
  * * The number is less than 12
  * * 7 does not divide the number
  * * 5 times the number is less than 70
  * Becky
  * * 12 times the number is greater than 1000
  * * 10 divides the number
  * * The number is greater than 100
  * Chloe
  * * 4 divides the number
  * * 11 times the number is less than 1000
  * * 9 divides the number
  * Danny
  * * The number is less than 20
  * * The number is prime
  * * 7 divides the number
  */
object Calc26 extends App {
  def isPrime(i: Int): Boolean =
    if (i <= 1) false
    else if (i == 2) true
    else !(2 until i).exists(x => i % x == 0)

  type Rule = Int => Boolean

  val rules = Map(
    "Andy" -> Seq[Rule]((x: Int) => x < 12, (x: Int) => (x % 7) != 0, (x: Int) => 5 * x < 70),
    "Becky" -> Seq[Rule]((x: Int) => 12 * x > 1000, (x: Int) => (x % 10) == 0, (x: Int) => x > 100),
    "Chloe" -> Seq[Rule]((x: Int) => (x % 4) == 0, (x: Int) => 11 * x < 1000, (x: Int) => (x % 9) == 0),
    "Danny" -> Seq[Rule]((x: Int) => x < 20, (x: Int) => isPrime(x), (x: Int) => (x % 7) == 0)
  )

  def check(x: Int): Boolean = {
    rules.values.forall { rulesList =>
      rulesList.exists(rule => rule(x)) && rulesList.exists(rule => !rule(x))
    }
  }
  println(Stream.from(0).filter(check).head)
}

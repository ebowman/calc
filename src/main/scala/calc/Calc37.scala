package calc

/**
  * Which of these statements are truths and which are lies?
  *
  * 1. Statement 2 and Statement 5 are either both truths or both lies.
  * 2. Statement 3 and Statement 5 are either both truths or both lies.
  * 3. Exactly two of the statements are truths.
  * 4. Statement 1 and Statement 2 are either both truths or both lies.
  * 5. Statement 3 is a lie.
  */
object Calc37 extends App {

  def rule1(seq: Seq[Boolean]): Boolean = seq(1) && seq(4) || !seq(1) && !seq(4)

  def rule2(seq: Seq[Boolean]): Boolean = seq(2) && seq(4) || !seq(2) && !seq(4)

  def rule3(seq: Seq[Boolean]): Boolean = seq.count(_ == true) == 2

  //noinspection ZeroIndexToHead
  def rule4(seq: Seq[Boolean]): Boolean = seq(0) && seq(1) || !seq(0) && !seq(1)

  def rule5(seq: Seq[Boolean]): Boolean = !seq(2)

  val rules = Seq(rule1 _, rule2 _, rule3 _, rule4 _, rule5 _)

  val inputs = for {
    x <- Seq(true, false)
    y <- Seq(true, false)
    z <- Seq(true, false)
    p <- Seq(true, false)
    q <- Seq(true, false)
  } yield Seq(x, y, z, p, q)

  val results = for {
    input <- inputs
  } yield {
    input.zip { rules.map(_(input)) }
  }
  results.filter(result => result.count(a => a._1 == a._2) == 5).foreach(println)
}

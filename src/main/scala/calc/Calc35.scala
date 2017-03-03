package calc

/**
  * If exactly one of these statements is false, which statement is false?
  *
  * A) Statement D is true.
  * B) Statement A is false.
  * C) Statement B is false.
  * D) Statement C is true.
  */
object Calc35 extends App {

  val mask = Seq(false, true, true, true)
  def a(test: Seq[Boolean]): Boolean = test(3)
  def b(test: Seq[Boolean]): Boolean = !test.head
  def c(test: Seq[Boolean]): Boolean = !test(1)
  def d(test: Seq[Boolean]): Boolean = test(2)
  val tests = Seq(a _, b _, c _, d _)

  for {
    perm <- mask.permutations
  } {
    val truths = tests.map(_(perm))
    val zips = truths.zip(perm)
    if (zips.count(a => a._1 == a._2) == 4 && zips.count(a => !a._1) == 1) {
      println(s"answer = ${('A' + zips.indexOf((false, false))).toChar}")
    }
  }

}

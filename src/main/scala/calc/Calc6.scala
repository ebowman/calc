package calc

object Calc6 extends App {

  val perms = (1 to 9).permutations

  def test(p: IndexedSeq[Int]): Boolean =
    p(0) + p(1) - p(2) == 6 &&
      p(0) + p(3) - p(6) == 4 &&
      (p(3) - p(4)) * p(5) == 8 &&
      p(6) * p(7) / p(8) == 3 &&
      (p(1) - p(4)) * p(7) == 3 &&
      p(2) * p(5) / p(8) == 4

  perms.filter(test).foreach { p =>
    println(p(0) * p(2) * p(6) * p(8))
  }

}

package calc

/**
  * Created by ebowman on 18/02/2017.
  */
object Calc3 extends App with Ops {
  val perms = (0 to 9).permutations

  def test(perm: IndexedSeq[Int]): Int = {
    val str = s"${perm(0)} + ${perm(1)}${perm(2)} + ${perm(3)}${perm(4)}${perm(5)} + ${perm(6)}${perm(7)}${perm(8)}${perm(9)}"
    parseAll(expr, str).get
  }

  //println(perms.map(test).max)
  println(perms.count(test(_) == 10656))

}

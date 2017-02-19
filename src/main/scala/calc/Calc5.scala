package calc

object Calc5 extends App with Ops2 {

  val perms: Iterator[IndexedSeq[Int]] = (1 to 5).permutations

  def make(perm: IndexedSeq[Int]): Double =
    parseAll(expr, s"${perm(0)} + ${perm(1)} - ${perm(2)} * ${perm(3)} / ${perm(4)}").get

  println(perms.map(make).min)
}

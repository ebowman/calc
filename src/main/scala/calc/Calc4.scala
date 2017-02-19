package calc

object Calc4 extends App {

  val perms = (1 to 4).permutations

  def compute(perm: IndexedSeq[Int]): Double = math.pow(perm(0), math.pow(perm(1), math.pow(perm(2), perm(3))))

  println(perms.count(compute(_) == 1))
}

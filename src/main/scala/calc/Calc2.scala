package calc

import scala.collection.immutable.IndexedSeq

object Calc2 extends App {

  val nums: Iterator[IndexedSeq[Int]] = (0 to 9).permutations

  def rule(set: IndexedSeq[Int]): Boolean = {
    val a = set(0) * set(1) * set(2)
    val b = set(1) * set(6) * set(4)
    val c = set(3) * set(4) * set(5)
    a == b && b == c
  }

  println(nums.find(rule))

}

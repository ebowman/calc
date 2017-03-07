package calc

import scala.collection.immutable.IndexedSeq

/**
  * A magic square of order 4 is created by putting the integers 1 to 16 into a 4 by 4 square grid so that the sum of
  * the numbers in each row, column and main diagonal is the same. What is the sum of the integers at the corners of
  * this magic square (labeled by letters as shown above)? (A, B, C, D)
  * So ... the brute force didn't work, this is a huge search space (16! = 20,922,789,888,000).
  * Searching the entire space would take about 500,000 years, but running for about a day (80,868,000,000 magic
  * squares, approximately) it stumbled upon the first magic square, and computed the answer (34).
  */
object Calc36 extends App {
  //noinspection ZeroIndexToHead
  def isMagic(s: IndexedSeq[Int]): Boolean = {
    val sum1 = s.slice(0, 4).sum
    val sum2 = s.slice(4, 8).sum
    if (sum1 == sum2) {
      val rowSums = IndexedSeq[Int](sum1, sum2, s.slice(8, 12).sum, s.slice(12, 16).sum)
      if (rowSums.distinct.length == 1) {
        // so far so good, continue
        val sum = rowSums.head
        val colSums = IndexedSeq(
          s(0) + s(4) + s(8) + s(12),
          s(1) + s(5) + s(9) + s(13),
          s(2) + s(6) + s(10) + s(14),
          s(3) + s(7) + s(11) + s(15)
        )
        if (colSums.head == sum && colSums.distinct.size == 1) {
          if (s(0) + s(5) + s(10) + s(15) == sum && s(3) + s(6) + s(9) + s(12) == sum) true
          else false
        } else false
      } else false
    } else false
  }

  val nums = 1 to 16
  var left = BigInt(16) * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2
  var done = BigInt(0)
  val buffer = 1000000
  val perms = nums.permutations.grouped(buffer)
  var start = System.currentTimeMillis()
  val magicIter = for {
    group <- perms
    _ = {
      val now = System.currentTimeMillis()
      left -= buffer
      done += buffer
      val ratePerMs = BigDecimal(done) / (now - start)
      val msRemaining = ratePerMs * BigDecimal(left)
      val years = msRemaining / (1000 * 60 * 60 * 24 * 365.25)
      println(s"done = $done remaining = $left years = $years")
    }
    perm <- group.par if isMagic(perm)
  } yield perm

  val magic = magicIter.next
  println(magic(0) + magic(3) + magic(12) + magic(15))
}

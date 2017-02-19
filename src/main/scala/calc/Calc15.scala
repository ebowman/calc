package calc

/**
  * Five points are located on a line. The ten distances between the pairs of points are listed from smallest to largest:
  * *
  * 2, 4, 5, 7, 8, K, 13, 15, 17, 19
  * *
  * Determine K
  */
object Calc15 extends App {

  val prefix = Seq(2, 4, 5, 7, 8)
  val postfix = Seq(13, 15, 17, 19)
  for {
    k <- 9 to 12
    s = (prefix :+ k) ++ postfix
    perm <- s.permutations
    a = 0
    b = perm.head
    c = b + perm(1)
    d = c + perm(2)
    e = d + perm(3) if Set(b - a, c - a, d - a, e - a, c - b, d - b, e - b, d - c, e - c, e - d) == s.toSet
  } {
    println(k)
  }
}

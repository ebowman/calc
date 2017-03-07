package calc

/**
  *   S E N D
  * + M O R E
  * ---------
  * M O N E Y
  *
  * solve for S + E + N + D + M + O + R + Y
  *
  *  9567
  * +1085
  * ---------
  * 10652
  *
  * Vector(38)
  */
object Calc38 extends App {

  val result = for {
    s <- 1 to 9
    e <- 0 to 9 if e != s
    n <- 0 to 9 if n != e && n != s
    d <- 0 to 9 if d != n && d != n && d != e && d != s
    m <- 1 to 9 if m != d if m != d && m != n && m != e && m != s
    o <- 0 to 9 if o != m if o != m && o != d && o != n && o != e && o != s
    r <- 0 to 9 if r != o && r != m && r != d && r != n && r != e && r != s
    y <- 0 to 9 if y != r && y != o && y != m && y != d && y != n && y != e && y != s &&
    m * 10000 + o * 1000 + n * 100 + e * 10 + y ==
      (s * 1000 + e * 100 + n * 10 + d) + (m * 1000 + o * 100 + r * 10 + e)
  } yield {
    println(
      s"""
         | $s$e$n$d
         |+$m$o$r$e
         |---------
         |$m$o$n$e$y
       """.stripMargin)
    s + e + n + d + m + o + r + y
  }
  println(result)
}

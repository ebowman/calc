package calc

/*
A maths professor asked one of his students to visit his house regarding some work. The student asked the
professor his house number. The professor replied in a strange way:

If my house number is a multiple of 3, then it is a number from 50 to 59.

If my house number is not a multiple of 4, then it is a number from 60 to 69.

If my house number is not a multiple of 6, then it is a number from 70 to 79.

Can you determine the professor's house number?
  */
object Calc29 extends App {

  def test1(x: Int): Boolean = if ((x % 3) == 0) { x >= 50 && x <= 59 } else true
  def test2(x: Int): Boolean = if ((x % 4) != 0) { x >= 60 && x <= 69 } else true
  def test3(x: Int): Boolean = if ((x % 6) != 6) { x >= 70 && x <= 79 } else true
  def test(x: Int) = test1(x) && test2(x) && test3(x)
  for { i <- 1 to 999 if test(i) } println(i)
}

package calc

/**
  * Brute force solver for the problem at https://brilliant.org/practice/arithmetic-puzzles-level-2-challenges/?p=2
  *
  * In a nutshell, given 1 _ 2 _ 3 _ 4, and the operators +, -, *, /, how many ways can you construct
  * a simple expression such that it evaluates to 10? (And for extra credit, 1 _ 2 _ 3 _ 4 _ 5 => 15).
  * Of course you can solve this by being smart, but this shows how to write simple program to solve it in
  * a brute-force way.
  *
  * Output:
  * For 1 _ 2 _ 3 _ 4, there are 2 solutions:
  * 1 + 2 + 3 + 4
  * 1 * 2 * 3 + 4
  * For 1 _ 2 _ 3 _ 4 _ 5, there are 3 solutions:
  * 1 + 2 + 3 + 4 + 5
  * 1 - 2 * 3 + 4 * 5
  * 1 * 2 * 3 + 4 + 5
  */
object Calc extends App with Ops {

  val ops = Seq("+", "-", "*", "/")

  val soln4 = for {
    op1 <- ops
    op2 <- ops
    op3 <- ops
    input = s"1 $op1 2 $op2 3 $op3 4" if parseAll(expr, input).get == 10
  } yield input

  println(s"For 1 _ 2 _ 3 _ 4, there are ${soln4.size} solutions:\n${soln4.mkString("\n")}")

  val soln5 = for {
    op1 <- ops
    op2 <- ops
    op3 <- ops
    op4 <- ops
    input = s"1 $op1 2 $op2 3 $op3 4 $op4 5" if parseAll(expr, input).get == 15
  } yield input

  println(s"For 1 _ 2 _ 3 _ 4 _ 5, there are ${soln5.size} solutions:\n${soln5.mkString("\n")}")
}
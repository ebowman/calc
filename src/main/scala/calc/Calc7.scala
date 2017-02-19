package calc

object Calc7 extends App with Ops {
  val ops = Seq("+", "-")
  val exprs = for {
    op1 <- ops
    op2 <- ops
    op3 <- ops
    op4 <- ops
    op5 <- ops
    op6 <- ops
    op7 <- ops
  } yield s"1 $op1 2 $op2 3 $op3 4 $op4 5 $op5 6 $op6 7 $op7 8"

  println(exprs.count { ex =>
    print(ex)
    print(" = ")
    println(parseAll(expr, ex).get)
    parseAll(expr, ex).get == 9
  })
}

package calc

/*
  */
object Calc39 extends App {

  for {
    a <- Seq(true, false)
    b <- Seq(true, false)
    c <- Seq(true, false) if a || b || c
  } println((a,b,c))

}

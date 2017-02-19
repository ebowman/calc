package calc

object Calc10 extends App {

  def f(n: Int)(k: BigInt): BigInt = {
    if (n == 1) {
      def sq(x: Int) = x*x
      k.toString.map(x => sq(x.toString.toInt)).sum
    } else {
      f(1)(f(n-1)(k))
    }
  }

  println(f(1)(2016))
  println(f(2)(2016))
  println(f(3)(2016))
  println(f(2017)(2016))
}

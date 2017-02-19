package calc

object Calc9 extends App {

  val stream = Stream.from(1).flatMap { _.toString }

  println(stream.take(28383).last)
}

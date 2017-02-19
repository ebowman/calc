package calc

import scala.util.parsing.combinator.JavaTokenParsers

trait Ops2 extends JavaTokenParsers {
  def num: Parser[Double] = wholeNumber ^^ (_.toDouble)

  def expr: Parser[Double] = num ~ rep(("*" | "/" | "+" | "-") ~ num) ^^ {
    case n ~ list => list.foldLeft(n) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
}

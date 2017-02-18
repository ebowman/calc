package calc

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * "Simplest possible" parser for trivial, integer expression parsing. See OpsSpec for what it can do.
  */
trait Ops extends JavaTokenParsers {
  def num: Parser[Int] = wholeNumber ^^ (_.toInt)

  def term: Parser[Int] = num ~ rep(("*" | "/") ~ num) ^^ {
    case n ~ list => list.foldLeft(n) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  def expr: Parser[Int] = term ~ rep(("+" | "-") ~ term) ^^ {
    case n ~ list => list.foldLeft(n) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
}

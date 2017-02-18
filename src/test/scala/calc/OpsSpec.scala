package calc

import org.scalatest.{FlatSpec, Matchers}

/**
  * Tests the expression evaluator.
  */
class OpsSpec extends FlatSpec with Ops with Matchers {


  /** num, term & expr should all be able to pass this test. */
  def testNums(p: Parser[Int]): Unit = {
    def f(str: String) = parseAll(p, str).get

    f("0") shouldBe 0
    f("10") shouldBe 10
  }

  /** term and expr should be able to pass this test. */
  def testTerms(p: Parser[Int]): Unit = {
    def f(str: String) = parseAll(p, str).get

    f("1 * 2") shouldBe 2
    f("0 * 10") shouldBe 0
    f("10 * 100") shouldBe 1000
    f("0") shouldBe 0
    f("10") shouldBe 10
    f("1 * 2") shouldBe 2
    f("0 * 10") shouldBe 0
    f("10 * 100") shouldBe 1000
    f("2 * 3 * 4") shouldBe 24
    f("9 / 1") shouldBe 9
    f("9 / 3") shouldBe 3
    f("9 / 3 / 3") shouldBe 1
    f("12 * 10 / 10") shouldBe 12
  }

  /* expr should be able to pass this test. */
  def testExpr(p: Parser[Int]): Unit = {
    def f(str: String) = parseAll(p, str).get

    f("1 + 2") shouldBe 3
    f("1 - 2") shouldBe -1
    f("1 + 2 - 3") shouldBe 0
    f("1 + 2 * 3") shouldBe 7
    f("1 + 2 * 3 / 4") shouldBe 2
  }

  "Numbers" should "be parsable" in {
    testNums(num)
  }

  "Terms" should "be parsable" in {
    testNums(term)
    testTerms(term)
  }

  "Expressions" should "be parsable" in {
    testNums(expr)
    testTerms(expr)
    testExpr(expr)
  }
}
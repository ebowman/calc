package calc

// https://brilliant.org/practice/grid-puzzles-level-2-challenges/?subtopic=puzzles&chapter=grid-puzzles

object Calc21 extends App {

  val digits = 1 to 8

  val result = for {
    perm <- digits.permutations if perm(0) + perm(1) == perm(2) &&
    perm(3) / perm(4) == perm(5) &&
    perm(0) + perm(3) == perm(6) &&
    perm(1) * perm(4) == perm(7)
  } yield perm

  result.foreach(println)
}

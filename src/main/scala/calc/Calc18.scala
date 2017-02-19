package calc

/**
  * One time, my brother stole my wallet and put it behind a door. Then he laid out 5 colored keys,
  * only one of them unlocking the door. He promises to return my wallet if I get the correct key.
  * Also, keys can talk about themselves. Using the clues below, can you find the correct key and find my wallet?
  *
  * The Red key is somewhere to the left of the key to the door.
  *
  * The Blue key is not at one of the ends.
  *
  * The Green key is three spaces away from the key to the door (2 between).
  *
  * The Yellow key is next to the key to the door.
  *
  * The Orange key is in the middle.
  */
object Calc18 extends App {
  val keys = Seq("R", "B", "G", "Y", "O")

  type Rule = (Seq[String], String) => Boolean

  val rules = Seq(
    (s: Seq[String], c: String) => s.indexOf("R") < s.indexOf(c),
    (s: Seq[String], c: String) => s.head != "B" && s.last != "B",
    (s: Seq[String], c: String) => {
      val idx = s.indexOf("G")
      (idx >= 3 && s(idx - 3) == c) || (idx <= 1 && s(idx + 3) == c)
    },

    (s: Seq[String], c: String) => {
      val idx = s.indexOf("Y")
      (idx > 0 && s(idx - 1) == c) || (idx < 4 && s(idx + 1) == c)
    }
  )

  val result = for {
    perm <- keys.permutations if perm(2) == "O"
    c <- keys if rules.forall(rule => rule(perm, c))
  } yield c
  println(result.toList)
}

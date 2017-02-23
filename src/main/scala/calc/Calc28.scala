package calc

/*
In the game Mastermind, I have to guess some permutation of five colors out of eight (being a permutation,
all colors are different). I can submit a guess, and I receive a reply in the form of two numbers:

The blue number tells the number of colors in my guess that exist in the secret pattern, which are also
  correct in position.
The white number tells the number of colors in my guess that exist in the secret pattern, but which are
  incorrect in position.
  
When where there should be a number, it is empty instead, the number is assumed to be zero.

What is the secret pattern? Enter your answer in order from left to right, translating the color to a digit code according to this table:

Color	Green	Blue	Yellow	White
Code	1	2	3	4
Color	Black	Red	Purple	Orange
Code	5	6	7	8
As an example, if the answer is orange-blue-yellow-white-black, enter 82345.
  */
object Calc28 extends App {

  val colors = Seq("Green", "Blue", "Yellow", "White", "Black", "Red", "Purple", "Orange")
  val played = Seq(
    ((0, 1), Seq("Purple", "Purple", "Purple", "Black", "Black")),
    ((1, 0), Seq("Black", "Black", "White", "White", "White")),
    ((0, 0), Seq("Yellow", "Black", "Yellow", "Yellow", "Yellow")),
    ((1, 2), Seq("Green", "Green", "White", "Purple", "Green")),
    ((1, 4), Seq("Green", "Blue", "Purple", "White", "Orange")),
    ((0, 5), Seq("Orange", "Blue", "Green", "Purple", "White"))
  )

  def check(colors: Seq[String], score: (Int, Int), play: Seq[String]): Boolean = {
    val perfect = score._1
    val near = score._2
    val test = colors.zip(play)
    val (perfects, nears) = test.partition { case (x, y) => x == y }
    if (perfect == perfects.size) {
      val nearColors = nears.map(_._1).toSet
      val nearPlay = nears.map(_._2).toSet
      nearPlay.intersect(nearColors).size == near
    } else false
  }

  def checkAll(colors: Seq[String]): Boolean = played.forall(x => check (colors, x._1, x._2))

  val result = for {
    perm <- colors.permutations if checkAll(perm.take(5))
  } yield perm
  result.foreach(println)
}

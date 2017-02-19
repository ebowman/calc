package calc

/*
Pop Quiz! Submit your answers, in order, to Questions 1-4.

Q1. Which is the first question where c) is the correct answer? a) Q3 b) Q4 c) Q1 d) Q2
Q2. Which is the first question where a) is the correct answer? a) Q4 b) Q2 c) Q3 d) Q1
Q3. Which is the first question where d) is the correct answer? a) Q1 b) Q2 c) Q4 d) Q3
Q4. Which is the first question where b) is the correct answer? a) Q2 b) Q4 c) Q3 d) Q1
 */
object Calc19 extends App {

  val answers = 0 to 3

  val questions = Seq(
    2 -> Seq(2, 3, 0, 1),
    0 -> Seq(3, 1, 2, 0),
    3 -> Seq(0, 1, 3, 2),
    1 -> Seq(1, 3, 2, 0)
  )

  def check(answer: Seq[Int]): Boolean =
    questions.zip(answer).forall { case ((q, answ), thisAnsw) => answer(answ(thisAnsw)) == q }

  for {
    answer <- answers.permutations if check(answer)
  } {
    println(answer)
  }

}

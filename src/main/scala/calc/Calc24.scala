package calc

/**
  * Sandip, Tracy, Jamal, and Sheng are best friends who work together everyday at a banana stand.
  * Since they are so close, their happiness on any given day depends on the happiness of the other three people
  * on the previous day. Suppose they behave as follows:
  *
  * 1. Sheng is happy today only if Tracy and Jamal were both happy yesterday.
  * 2. Jamal is happy today only if Sandip or Sheng (or both) were happy yesterday.
  * 3. Sandip enjoys watching Sheng cry, so Sandip is happy today only if Sheng was sad yesterday.
  * 4. Tracy is happy today only if Tracy was happy yesterday, meaning she has an independent streak.
  *
  * Suppose that on day 1, all four of the friends are sad. After a few days, the friends reach a stable emotional
  * state that repeats itself. What is the emotional state of each person in this repeating state?
  */
object Calc24 extends App {

  type State = (Boolean, Boolean, Boolean, Boolean)

  def evolve(yesterday: State): State = {
    val Sandip = yesterday._1
    val Tracy = yesterday._2
    val Jamal = yesterday._3
    val Sheng = yesterday._4
    (!Sheng, Tracy, Sandip || Sheng, Tracy && Jamal )
  }

  val initial = (false, false, false, false)

  val stream: Stream[State] = {
    def next(state: State): Stream[State] = {
      state #:: next(evolve(state))
    }
    next(initial)
  }

  stream.take(20).toList.foreach(println)
}

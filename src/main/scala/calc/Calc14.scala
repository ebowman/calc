package calc

/**
  *
  * You are the village chief. Your village has 1000 residents including yourself. One day the ferocious EVEN DEMON
  * attacks your village and snarls, "All you 1000 villagers stand in a large CIRCLE, numbering yourself from 1 to 1000.
  * Starting from number 1, I will eat every second villager (which means he will eat villager number 2, then villager
  * number 4 and so on) and keep going around and around the circle eating up every second villager and keep doing this
  * till there is only 1 villager left. That last villager I will spare and he is free to escape."
  *
  * Since you are the village chief, you have the right to choose where you wish to stand.
  *
  * In the original circle of 1000 villagers which number will you choose to stand at to be the last villager standing
  * and escape the clutches of the EVEN DEMON?
  *
  * This problem is not an original. It is adapted from a famous problem recorded by a Jewish historian.
  */
object Calc14 extends App {

  def doCircle(circle: Array[Int]): Array[Int] = {
    println(circle.toSeq)
    var done = false
    var cursor = 1
    var prevEaten = -1
    while (!done) {
      println(s"eat ${circle(cursor)}")
      prevEaten = circle(cursor)
      circle(cursor) = -1
      if (cursor + 2 == circle.length) {
        println(s"*eat ${circle(0)}")
        circle(0) = -1
        done = true
      }
      else if (cursor + 2 >= circle.length) done = true
      else cursor += 2
    }
    val nextCircle = circle.filterNot(_ == -1)
    if (nextCircle.length == 1) {
      nextCircle //Array(prevEaten)
    } else doCircle(nextCircle)
  }

  println(doCircle((1 to 1000).toArray).toSeq.head)
}

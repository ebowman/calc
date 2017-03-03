package calc

/**
  * There are 3 boxes, exactly one of which has a car. You can keep the car if you pick the correct box!
  *
  * On each box there is a statement, exactly one of which is true.
  *
  * Box 1: The car is in this box.
  * Box 2: The car is not in this box.
  * Box 3: The car is not in box 1.
  *
  * Which box has the car?
  */
object Calc33 extends App {

  val boxes = Seq(true, false, false)

  def rule(seq: Seq[Boolean]) = Seq(seq.head, !seq(1), !seq.head)

  for {
    perm <- boxes.permutations if rule(perm).count(_ == true) == 1
  } println(perm)
}

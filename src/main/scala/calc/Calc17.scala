package calc

/**
  * Last night I went with my wife to a party where four other married couples were present.
  *
  * Every person shook hands with only the people he or she was NOT acquainted with.
  * (Obviously, no one shook his or her own hand or spouse's hand, and no one shook hands with the same person twice.)
  *
  * When the handshaking was over, I asked everyone including my wife how many hands they each shook.
  *
  * To my surprise, I got 9 different answers!
  *
  * How many hands did my wife shake?
  *
  * This problem is not an original. It is adapted from a problem posed by Martin Gardener.
  */
object Calc17 extends App {
  val people = Seq("h1", "w1", "h2", "w2", "h3", "w3", "h4", "w4", "h5", "w5")
  /*
  case class Person(name: String, known: Set[Person] = Set.empty)

  val foo = for {
    personId <- people.indices
    person = people(personId)
    candidates = people.drop(personId + 1).filterNot(maybeSpouse => maybeSpouse(1) == person(1))
    n <- 0 to candidates.length
    comb <- candidates.combinations(n)
  } yield person -> comb.toSet

  val folded = foo.foldLeft(Map.empty[String, Set[Set[String]]]) {
    case (map, item) =>
      if (map.contains(item._1)) map + (item._1 -> (map(item._1) + item._2))
      else map + (item._1 -> Set(item._2))
  }

  println(folded)
  folded.foreach(println)
  println(foo.size)
  */

  case class RelMat(m: Seq[Boolean])

  object RelMat {
    def apply(v: BigInt): Option[RelMat] = {
      val prod = people.size * people.size
      val m = new Array[Boolean](prod)
      for (i <- 0 until prod) {
        if ((v & (BigInt(1) << i)) == 0) m(i) = true
      }
      for (i <- people.indices) m(i * people.size) = false
      for (i <- people.indices by 2) {
        m(i * people.size + 1) = false
        m((i + 1) * people.size - 1) = false
      }
      val counts = for (i <- people.indices) yield m.slice(i * people.size, (i + 1) * people.size).count(_ == true)
      if (counts.size == counts.distinct.size) Some(RelMat(m.toSeq))
      else None
    }
  }

  

//  val str1 = Stream.
//  println((BigInt(0) to BigInt(1) << 100).toStream.map(RelMat.apply).head)

}

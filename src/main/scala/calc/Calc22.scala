package calc

/**
  * One evening there was a murder in the home of a father, a mother, their son, and their daughter.
  *
  * One of these four people murdered one of the others. One other member of the family witnessed the crime.
  * The other one helped the murderer.
  *
  * These are the things we know for sure:
  * MWmH
  * The witness and the one who helped the murderer were not of the same sex.
  * The oldest person and the witness were not of the same sex.
  * The youngest person and the victim were not of the same sex.
  * The one who helped the murderer was older than the victim.
  * The father was the oldest member of the family.
  * The murderer was not the youngest member of the family.
  * Who was the murderer?
  */
object Calc22 extends App {
  // father=0, mother=1, son=2, daughter=3
  // M, W, H, m (murderer, witness, helper, murdered)

  val states = Seq("M", "W", "H", "m")
  val sexes = Seq(0, 1, 0, 1)

  // The witness and the one who helped the murderer were not of the same sex.
  def rule1(state: Seq[String], youngest: String): Boolean = sexes(state.indexOf("W")) != sexes(state.indexOf("H"))


  // The oldest person and the witness were not of the same sex.
  def rule2(state: Seq[String], youngest: String): Boolean = state.head != "W" && sexes.head != sexes(state.indexOf("W"))


  // The youngest person and the victim were not of the same sex.
  def rule3(state: Seq[String], youngest: String): Boolean = {
    youngest match {
      case "son" => sexes(state.indexOf("m")) == 1
      case "daughter" => sexes(state.indexOf("m")) == 0
    }
  }

  // The one who helped the murderer was older than the victim.
  def rule4(state: Seq[String], youngest: String): Boolean = {
    val helper = state.indexOf("H")
    val victim = state.indexOf("m")
    youngest match {
      case "son" =>
        helper == 0 || helper == 1 || helper == 3
      case "daughter" =>
        helper == 0 || helper == 1 || helper == 2
    }
  }

  // The murderer was not the youngest member of the family.
  def rule5(state: Seq[String], youngest: String): Boolean = {
    val murderer = state.indexOf("M")
    youngest match {
      case "son" => murderer != 2
      case "daughter" => murderer != 3
    }
  }

  val allRules = Seq(rule1 _, rule2 _, rule3 _, rule4 _, rule5 _)

  val results = for {
    youngest <- Seq("son", "daughter")
    state <- states.permutations if allRules.forall(_.apply(state, youngest))
  } yield state

  results.foreach(println)
}

package calc

/**
  * John tells the truth on Mondays, Thursdays and Saturdays, but lies on every other day.
  *
  * One day he said, “I will tell the truth tomorrow.”
  *
  * On which day of the week did he make this statement?
  */
object Calc32 extends App {

  object Days extends Enumeration {
    type Days = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  import Days._

  val nextDays = Map(Mon -> Tue, Tue -> Wed, Wed -> Thu, Thu -> Fri, Fri -> Sat, Sat -> Sun, Sun -> Mon)

  val trueDays = Set(Mon, Thu, Sat)

  Days.values.find(day => trueDays(day) && trueDays(nextDays(day)) || !trueDays(day) && !trueDays(nextDays(day))).foreach(println)
}

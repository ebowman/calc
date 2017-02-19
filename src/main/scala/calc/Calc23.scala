package calc

/**
  * Aaron, Calvin, David, and Peter each live in one of 4 adjacent townhouses in a row, each of a single color.
  * Each owns one pet and imbibes one kind of drink.
  *
  * Aaron owns the dog.
  * The bird lives in the red house.
  * Calvin lives in the blue house.
  * David does not live in the red house.
  * The cat lives where the milk drinker lives.
  * Either the fish lives next to the cat or the bird lives next to the coffee drinker.
  * If the dog lives in the green house, then the cat lives next to the blue house.
  * If Peter owns the fish, then either Calvin owns the bird or else David owns the cat.
  * The tea drinker lives two houses away from the coffee drinker.
  * The red house resident drinks water if and only if the yellow house resident drinks milk.
  * Who owns the fish?
  *
  * Note: Color of residences as shown in photograph have nothing to do with this problem.
  * Also, any pet "owned" is presumed to live in the same place as the owner lives.
  */
object Calc23 extends App {

  type Rule = (Seq[String], Seq[String], Seq[String], Seq[String]) => Boolean

  val rules = Seq[Rule](
    // Aaron owns the dog.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) =>
      pets(names.indexOf("Aaron")) == "dog",
    // The bird lives in the red house.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) =>
      pets(colors.indexOf("red")) == "bird",
    //Calvin lives in the blue house.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) =>
      names(colors.indexOf("blue")) == "Calvin",
    // David does not live in the red house.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) =>
      colors(names.indexOf("David")) != "red",
    // The cat lives where the milk drinker lives.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) =>
      pets.indexOf("cat") == drinks.indexOf("milk"),
    // Either the fish lives next to the cat or the bird lives next to the coffee drinker.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) => {
      math.abs(pets.indexOf("fish") - pets.indexOf("cat")) == 1 ||
        math.abs(pets.indexOf("bird") - drinks.indexOf("coffee")) == 1
    },
    // If the dog lives in the green house, then the cat lives next to the blue house.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) => {
      if (pets.indexOf("dog") == colors.indexOf("green")) math.abs(pets.indexOf("cat") - colors.indexOf("blue")) == 1
      else true
    },
    // If Peter owns the fish, then either Calvin owns the bird or else David owns the cat.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) => {
      if (names(pets.indexOf("fish")) == "Peter") {
        names.indexOf("Calvin") == pets.indexOf("bird") ||
          names.indexOf("David") == pets.indexOf("cat")
      } else true
    },
    // The tea drinker lives two houses away from the coffee drinker.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) => {
      math.abs(drinks.indexOf("tea") - drinks.indexOf("coffee")) == 2
    },
    // The red house resident drinks water if and only if the yellow house resident drinks milk.
    (names: Seq[String], colors: Seq[String], pets: Seq[String], drinks: Seq[String]) => {
      if (colors.indexOf("yellow") == drinks.indexOf("milk")) colors.indexOf("red") == drinks.indexOf("water")
      else colors.indexOf("red") != drinks.indexOf("water")
    }
  )

  val result = for {
    n <- Seq("Aaron", "Calvin", "David", "Peter").permutations
    c <- Seq("red", "blue", "green", "yellow").permutations
    p <- Seq("dog", "bird", "cat", "fish").permutations
    d <- Seq("milk", "coffee", "tea", "water").permutations if rules.forall(_.apply(n, c, p, d))
  } yield (n, c, p, d)

  //println(result.size)
  result.foreach { case (n, c, p, d) =>
    println((n,c,p,d))
    println(n(p.indexOf("fish")))
  }
}

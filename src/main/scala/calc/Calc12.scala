package calc

object Calc12 extends App {

  val pageNumbers = 1 to 512

  val allPossibilities = for {
    start <- 1 to 512
    end <- start + 1 to 512
  } yield (start to end).sum

  println(s"could be 512: ${allPossibilities.contains(512)}")
  println(s"could be 412: ${allPossibilities.contains(412)}")
}

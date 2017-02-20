package calc

/**
  * The picture above shows a Hidato puzzle. The aim of the puzzle is to fill each white/light blue cell with an
  * integer between 1 and 85 (inclusive) so that each integer appears exactly once and consecutive integers appear
  * in adjacent cells.
  * Let the number that takes place of the cell marked  be denoted , and so forth.
  *
  * What is the value of A + B + C?
  * https://d18l82el6cdm1i.cloudfront.net/image_optimizer/c09efaf9895260d968f45b354c51c2eda6d4f88f.png
  *
  * We'll use cube coordinates from here: http://www.redblobgames.com/grids/hexagons/
  */
object Calc27 extends App {

  val size = 5

  // top right = x, top left = y, bottom = z
  case class Coord(x: Int, y: Int, z: Int) {
    require(x + y + z == 0, (x, y, z))

    lazy val neighbors: Seq[Coord] = {
      Seq(e, w, ne, nw, se, sw).filter(_.inRange).filterNot(deadSet)
    }

    def inRange: Boolean = math.abs(x) <= size && math.abs(y) <= size && math.abs(z) <= size

    def e = Coord(x + 1, y - 1, z)

    def w = Coord(x - 1, y + 1, z)

    def nw = Coord(x, y + 1, z - 1)

    def ne = Coord(x + 1, y, z - 1)

    def sw = Coord(x - 1, y, z + 1)

    def se = Coord(x, y - 1, z + 1)
  }


  val origin = Coord(0, 0, 0)
  val deadSet = Set(origin.w.w, origin.w.w.nw, origin.w.w.ne, origin.e.e, origin.e.e.nw, origin.e.e.ne)
  val grid = {
    import scala.collection.mutable
    val map = mutable.Map[Coord, Int]()
    for {
      x <- -5 to 5
      y <- -5 to 5
      z <- -5 to 5 if x + y + z == 0
    } {
      map += Coord(x, y, z) -> 0
    }
    map += origin.w.w.w.w.w -> 81
    map += origin.nw.nw.nw.w.w -> 85
    map += origin.nw.nw.nw.nw -> 62
    map += origin.nw.nw.nw.ne.ne -> 57
    map += origin.ne.ne.ne -> 53
    map += origin.nw -> 31
    map += origin.w -> 29
    map += origin.sw.w.w.w -> 72
    map += origin.sw.sw.w -> 26
    map += origin.sw.sw.sw.se -> 23
    map += origin.se.se.se -> 1
    map += origin.se.se.se.sw -> 12
    map += origin.se.se.se.se -> 9
    map += origin.se.se.se.e.e -> 6
    map += origin.se.e.e -> 3
    map += origin.se.e.e.e -> 43
    map += origin.e.e.e.e -> 42

    deadSet.foreach(dead => map += dead -> -1)
    map.toMap
  }


  val a = origin.nw.nw.nw.nw.nw
  val b = origin.nw.nw
  val c = origin.nw.ne.ne

  type Grid = Map[Coord, Int]

  def recurse(grids: Seq[Grid], values: Seq[Int]): Seq[Grid] = {
    if (values.isEmpty) grids
    else {
      val value = values.head
      grids.flatMap { grid =>
        val gridValues = grid.values.toSet

        def legal(coord: Coord): Boolean = {
            coord.neighbors.exists(n => grid(n) == value - 1) &&
            (coord.neighbors.exists(n => grid(n) == value + 1) ||
              coord.neighbors.exists(n => grid(n) == 0 && !gridValues.contains(value + 1)))
        }

        val candidates: Seq[Coord] = grid.filter(_._2 == 0).keys.filter(legal).toSeq
        if (candidates.isEmpty) {
          Seq.empty
        }
        else {
          val newGrids = candidates.map(c => grid + (c -> value))
          recurse(newGrids, values.tail)
        }
      }
    }
  }

  val alreadyInserted = grid.values.toSet
  val toInsert = (1 to 85).filterNot(alreadyInserted)
  val start = System.currentTimeMillis()
  val results = recurse(Seq(grid), toInsert)
  val stop = System.currentTimeMillis()
  println(s"total time: ${stop - start}")
  results.foreach { grid =>
    println {
      "validity check: " +
      grid.forall { case (coord, value) =>
          if (value == 1) {
            coord.neighbors.exists(c => grid(c) == 2)
          } else if (value == 85) {
            coord.neighbors.exists(c => grid(c) == 84)
          } else {
            coord.neighbors.exists(c => grid(c) == value - 1) &&
              coord.neighbors.exists(c => grid(c) == value + 1)
          }
      }
    }
    println(s"A = ${grid(a)}")
    println(s"B = ${grid(b)}")
    println(s"C = ${grid(c)}")
    println(s"sum = ${grid(a) + grid(b) + grid(c)}")
  }
}

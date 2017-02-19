package calc

/**
  * In the diagram given above, the numbers from 1 to 25 are to arranged in a  square grid so that each number
  * except 1 and 2, is the sum of two of its neighbours. Some of the numbers have already been filled in.
  * Which number must replace the "  " when the grid is complete.
  *
  * ?? ?? ?? 20 21
  * ??  6  5  4 ??
  * 23  7  1  3  X
  * ??  9  8  2 ??
  * 25 25 ?? ?? 22
  *
  * Note - Numbers in the grid are neighbours if their squares touch along a side or at a corner. For example: the number "1" has eight neighbours, the number "21" has three neighbours.
  *
  * Bonus - Complete the Grid.
  */
object Calc20 extends App {

  def newGrid() = {
    val grid = new Array[Array[Int]](5)
    grid(0) = Array(-1, -1, -1, 20, 21)
    grid(1) = Array(-1, 6, 5, 4, -1)
    grid(2) = Array(23, 7, 1, 3, -1)
    grid(3) = Array(-1, 9, 8, 2, -1)
    grid(4) = Array(25, 24, -1, -1, 22)
    grid
  }

  def store(grid: Array[Array[Int]], coord: (Int, Int), value: Int): Unit = grid(coord._1)(coord._2) = value

  def testCoord(grid: Array[Array[Int]])(coord: (Int, Int)): Boolean = {
    val testValue = grid(coord._1)(coord._2)
    if (testValue <= 2) true
    else {
      val allCoords = for {
        y <- coord._1 - 1 to coord._1 + 1 if y >= 0 && y < 5
        x <- coord._2 - 1 to coord._2 + 1 if x >= 0 && x < 5 && (y, x) != coord
      } yield (y, x)

      allCoords.combinations(2).exists { coordPair =>
        grid(coordPair(0)._1)(coordPair(0)._2) +
          grid(coordPair(1)._1)(coordPair(1)._2) == testValue
      }
    }
  }

  val toFind = Seq((0, 0), (0, 1), (0, 2), (1, 0), (1, 4), (2, 4), (3, 0), (3, 4), (4, 2), (4, 3))
  val missing = Seq(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
  val allCoords = for { y <- 0 until 5; x <- 0 until 5 } yield (y, x)

  val grids: Iterator[Array[Array[Int]]] = for {
    ordering <- missing.permutations
    grid = newGrid()
    _ = toFind.zip(ordering).foreach { case (coord, value) => store(grid, coord, value)}
    if allCoords.forall(testCoord(grid))
  } yield grid

  def printGrid(grid: Array[Array[Int]]): Unit = {
    grid.foreach { row =>
      println("||" + row.mkString("||") + "||")
    }
  }

  grids.foreach { grid =>
    printGrid(grid)
    print("\n")
  }
}

package calc

import java.util.concurrent.atomic.AtomicInteger

import scala.util.Try

/**
  * What is the largest number of these tetrominoes​ which can fit on a 7x7 grid without any overlap?
  * The pieces can be rotated and reflected. However, they cannot overlap and go off the grid.
  */
object Calc31 extends App {

  type Point = (Int, Int)
  type Grid = Array[Array[Boolean]]

  val shapes = Seq(
    Seq((1, 0), (1, 1), (2, 1)),
    Seq((0, 1), (-1, 1), (-1, 2)),
    Seq((-1, 0), (-1, -1), (-2, -1)),
    Seq((0, -1), (1, -1), (1, -2)),
    Seq((1, 0), (1, -1), (2, -1)),
    Seq((0, 1), (1, 1), (1, 2)),
    Seq((-1, 0), (-1, 1), (-2, 1)),
    Seq((0, -1), (-1, -1), (-1, -2))
  )

  def test(pt: Point, idx: Int, grid: Grid): Boolean = {
    val x = pt._1
    val y = pt._2
    Try {
      !grid(y)(x) &&
        !grid(y + shapes(idx).head._2)(x + shapes(idx).head._1) &&
        !grid(y + shapes(idx)(1)._2)(x + shapes(idx)(1)._1) &&
        !grid(y + shapes(idx)(2)._2)(x + shapes(idx)(2)._1)
    } getOrElse false
  }

  def set(pt: Point, idx: Int, grid: Grid): Grid = {
    val x = pt._1
    val y = pt._2
    grid(y)(x) = true
    grid(y + shapes(idx).head._2)(x + shapes(idx).head._1) = true
    grid(y + shapes(idx)(1)._2)(x + shapes(idx)(1)._1) = true
    grid(y + shapes(idx)(2)._2)(x + shapes(idx)(2)._1) = true
    grid
  }

  val counter = new AtomicInteger(0)

  implicit class ArrayOps(val array: Grid) extends AnyRef {
    def dup: Grid = {
      val count = counter.incrementAndGet()
      if ((count % 10000) == 0) println(count)
      val a = new Grid(array.length)
      a.indices.foreach(y => a(y) = array(y).clone())
      a
    }

    def free: Int = array.map(_.count(_ == false)).sum

    def print: String = {
      array.map(row => row.map(a => if (a) "x" else " ").mkString(" ")).mkString("\n")
    }
  }


  def recurse(count: Int, grid: Grid): Seq[(Int, Grid)] = {
    if (grid.free < 10) Seq((count, grid)) // cheating to try to get to terminate early enough to complete
    else {
      val newGrids = for {
        y <- grid.indices.par
        x <- grid.indices if !grid(y)(x)
        i <- shapes.indices if test((x, y), i, grid)
      } yield set((x, y), i, grid.dup)

      newGrids.seq.flatMap(g => recurse(count + 1, g))
    }
  }

  def mkGrid(): Grid = {
    val grid = new Grid(7)
    grid.indices.foreach(y => grid(y) = new Array[Boolean](7))
    grid
  }

  val grid = mkGrid()
  val result = recurse(0, grid)
  val best = result.maxBy(_._1)
  println(best)
}

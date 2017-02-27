package calc

import scala.collection.immutable.Seq

/*
Imagine that you are walking along the lines of the grid of unit squares below.

-------------
|  |  |  |  |
-------------
|  |  |  |  |
-------------
|  |  |  |  |
-------------
|  |  |  |  |
-------------

If you start from the bottom left-hand corner and walk along the lines until you return to your starting point,
what is the length of the longest path you can make if you can't travel on the same line segment or pass through
the same point twice?
  */
object Calc30 extends App {

  type Vertex = (Int, Int)
  type Edge = (Vertex, Vertex)

  val edges: Set[Edge] = ((0 to 3).flatMap { x =>
    (0 to 3).flatMap { y =>
      Seq((x, y) -> (x + 1, y), (x, y) -> (x, y + 1))
    }
  } ++
    (for {x <- 0 to 3} yield (x, 4) -> (x + 1 -> 4)) ++
    (for {y <- 0 to 3} yield (4, y) -> (4, y + 1))).toSet

  val edgesByVertex: Map[Vertex, Set[Edge]] = {
    val group1 = edges.groupBy(_._1)
    val group2 = edges.groupBy(_._2)
    for (g <- group1.keys ++ group2.keys) yield g -> (group1.getOrElse(g, Seq.empty) ++ group2.getOrElse(g, Seq.empty))
  }.map(x => x._1 -> x._2.toSet).toMap

  def normalize(x: Edge): Edge = {
    if (x._1._1 > x._2._1 || x._1._2 > x._2._2) (x._2, x._1)
    else x
  }

  case class Path(seq: Seq[Vertex], remaining: Set[Edge]) {
    def print: String = {
      val grid = new Array[Array[Char]](15)
      grid.indices.foreach { y =>
        grid(y) = new Array[Char](15)
        grid(y).indices.foreach(x => grid(y)(x) = ' ')
      }
      /*
       - - - - - - -
      | | | | | | | |
       - - - - - - -
      | | | | | | | |
       - - - - - - -
      | | | | | | | |
       - - - - - - -
      | | | | | | | |
       - - - - - - -
      | | | | | | | |
       - - - - - - -
      | | | | | | | |
       - - - - - - -
      | | | | | | | |
       - - - - - - -
       */
      seq.zip(seq.tail).map(normalize).foreach {
        case ((x1, y1), (x2, y2)) if x1 == x2 => grid(14 - 2 * y2)(x1 * 2) = '|'
        case ((x1, y1), (x2, y2)) if y1 == y2 => grid(13 - 2 * y2)(2 * x1 + 1) = '-'
      }
      grid.map(_.mkString("")).mkString("\n")
    }
  }

  def recurse(path: Path): Set[Path] = {
    if (path.seq.size > 1 && path.seq.head == path.seq.last) {
      Set(path)
    } else if (!path.remaining.contains((0, 0) -> (0, 1)) && !path.remaining.contains((0, 0) -> (1, 0))) {
      Set.empty
    }
    else {
      for {
        nextEdge <- edgesByVertex(path.seq.last) if path.remaining.contains(nextEdge)
      } yield {
        if (path.seq.indexOf(nextEdge._1) == -1 || path.seq.size > 2 && nextEdge._1 == (0, 0)) {
          recurse(path.copy(seq = path.seq :+ nextEdge._1, remaining = path.remaining - nextEdge))
        } else if (path.seq.indexOf(nextEdge._2) == -1) {
          recurse(path.copy(seq = path.seq :+ nextEdge._2, remaining = path.remaining - nextEdge))
        } else {
          Seq.empty
        }
      }
    }.flatten
  }

  val result = recurse(Path(Seq((0, 0)), edges))
  result.foreach(r => {
    println(r.print)
    println()
  })
  println(result.count(_.seq.size == 25))

}

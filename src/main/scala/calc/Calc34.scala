package calc

/**
  * Assume that you are playing a Tic Tac Toe game with an opponent. You are playing as x while your opponent plays as o.
  *
  * The game is played as follows:
  *
  * | x| |   x|o|   x|o|   x|o|   x|o|a
  * | -----  -----  -----  -----  -----
  * |  | |    | |    | |   o| |   o|b|c
  * | -----  -----  -----  -----  -----
  * |  | |    | |   x| |   x| |   x|d|e
  *
  * Find all the possible grid tiles where you can put an x-mark will help you guarantee a win under best play.
  *
  * (answer is b or e, the question is, can we demonstrate this algorithmically).
  * It looks to me like c is a very good move as well, however -- I don't see how o can if I play perfectly from
  * initial position (c).
  *
  */
object Calc34 extends App {

  //noinspection ZeroIndexToHead
  case class Board(rows: Seq[Seq[Char]] = for (y <- 1 to 3) yield Seq(' ', ' ', ' ')) {
    def set(piece: Char, pos: (Int, Int)): Board = {
      require(piece == 'x' || piece == 'o')
      Board(rows.updated(pos._2, rows(pos._2).updated(pos._1, piece)))
    }

    def free(pos: (Int, Int)): Boolean = rows(pos._2)(pos._1) == ' '

    def frees: Seq[(Int, Int)] = for {
      y <- 0 until 3
      x <- 0 until 3 if free((x, y))
    } yield (x, y)

    def done: Boolean = win.nonEmpty || !rows.exists(row => row.contains(' '))

    def win: Option[Char] = {
      def equal3[Char](a: Char, b: Char, c: Char): (Boolean, Char) = (a == b && b == c && a != ' ', a)

      val acrossTop = equal3(rows(0)(0), rows(0)(1), rows(0)(2))
      val acrossMiddle = equal3(rows(1)(0), rows(1)(1), rows(1)(2))
      val acrossBottom = equal3(rows(2)(0), rows(2)(1), rows(2)(2))
      val downLeft = equal3(rows(0)(0), rows(1)(0), rows(2)(0))
      val downMiddle = equal3(rows(0)(1), rows(1)(1), rows(2)(1))
      val downRight = equal3(rows(0)(2), rows(1)(2), rows(2)(2))
      val diagLeft = equal3(rows(0)(0), rows(1)(1), rows(2)(2))
      val diagRight = equal3(rows(0)(2), rows(1)(1), rows(2)(0))

      val checks = Seq(acrossTop, acrossMiddle, acrossBottom, downLeft, downMiddle, downRight, diagLeft, diagRight)
      checks.find(_._1).map(_._2)
    }

    def winningMoves(piece: Char): Seq[(Int, Int)] = {
      val moves = new scala.collection.mutable.ListBuffer[(Int, Int)]

      if (rows(0)(0) == ' ' && rows(0)(1) == piece && rows(0)(2) == piece) moves.append((0, 0))
      if (rows(1)(0) == ' ' && rows(1)(1) == piece && rows(1)(2) == piece) moves.append((0, 1))
      if (rows(2)(0) == ' ' && rows(2)(1) == piece && rows(2)(2) == piece) moves.append((0, 2))

      if (rows(0)(0) == piece && rows(0)(1) == ' ' && rows(0)(2) == piece) moves.append((1, 0))
      if (rows(1)(0) == piece && rows(1)(1) == ' ' && rows(1)(2) == piece) moves.append((1, 1))
      if (rows(2)(0) == piece && rows(2)(1) == ' ' && rows(2)(2) == piece) moves.append((1, 2))

      if (rows(0)(0) == piece && rows(0)(1) == piece && rows(0)(2) == ' ') moves.append((2, 0))
      if (rows(1)(0) == piece && rows(1)(1) == piece && rows(1)(2) == ' ') moves.append((2, 1))
      if (rows(2)(0) == piece && rows(2)(1) == piece && rows(2)(2) == ' ') moves.append((2, 2))

      if (rows(0)(0) == ' ' && rows(1)(0) == piece && rows(2)(0) == piece) moves.append((0, 0))
      if (rows(0)(1) == ' ' && rows(1)(1) == piece && rows(2)(1) == piece) moves.append((1, 0))
      if (rows(0)(2) == ' ' && rows(1)(2) == piece && rows(2)(2) == piece) moves.append((2, 0))

      if (rows(0)(0) == piece && rows(1)(0) == ' ' && rows(2)(0) == piece) moves.append((0, 1))
      if (rows(0)(1) == piece && rows(1)(1) == ' ' && rows(2)(1) == piece) moves.append((1, 1))
      if (rows(0)(2) == piece && rows(1)(2) == ' ' && rows(2)(2) == piece) moves.append((2, 1))

      if (rows(0)(0) == piece && rows(1)(0) == piece && rows(2)(0) == ' ') moves.append((0, 2))
      if (rows(0)(1) == piece && rows(1)(1) == piece && rows(2)(1) == ' ') moves.append((1, 2))
      if (rows(0)(2) == piece && rows(1)(2) == piece && rows(2)(2) == ' ') moves.append((2, 2))

      if (rows(0)(0) == ' ' && rows(1)(1) == piece && rows(2)(2) == piece) moves.append((0, 0))
      if (rows(0)(0) == piece && rows(1)(1) == ' ' && rows(2)(2) == piece) moves.append((1, 1))
      if (rows(0)(0) == piece && rows(1)(1) == piece && rows(2)(2) == ' ') moves.append((2, 2))

      if (rows(2)(0) == ' ' && rows(1)(1) == piece && rows(0)(2) == piece) moves.append((0, 2))
      if (rows(2)(0) == piece && rows(1)(1) == ' ' && rows(0)(2) == piece) moves.append((1, 1))
      if (rows(2)(0) == piece && rows(1)(1) == piece && rows(0)(2) == ' ') moves.append((2, 0))

      moves.toSet.toSeq
    }

    def blockingMoves(piece: Char): Seq[(Int, Int)] = winningMoves(nextPiece(piece))

    def bestMove(piece: Char): (Int, Int) = {
      val wins = winningMoves(piece)
      if (wins.nonEmpty) wins.head
      else {
        val blocks = blockingMoves(piece)
        if (blocks.nonEmpty) blocks.head
        else {
          def recurse(piece: Char, board: Board): Seq[Board] = {
            if (board.win.nonEmpty) Seq(board)
            else if (board.done) Seq.empty
            else {
              board.frees.flatMap { case (x, y) =>
                recurse(nextPiece(piece), board.set(piece, (x, y)))
              }
            }
          }
          val outcomes: Seq[((Int, Int), Seq[Board])] = frees.map(pos => pos -> recurse(piece, this.set(piece, pos)))
          val wins = outcomes.map { case (pos, games) => (pos, games.count(_.win.contains(piece)))}.sortBy(_._2)
          wins.last._1
        }
      }
    }

    override def toString = {
      s"""
         ||${rows(0)(0)}|${rows(0)(1)}|${rows(0)(2)}|
         ||-----|
         ||${rows(1)(0)}|${rows(1)(1)}|${rows(1)(2)}|
         ||-----|
         ||${rows(2)(0)}|${rows(2)(1)}|${rows(2)(2)}|
       """.stripMargin
    }
  }

  def nextPiece(piece: Char): Char = piece match {
    case 'x' => 'o'
    case 'o' => 'x'
  }

  val initial = Board().set('x', (0, 0)).set('o', (1, 0)).set('o', (0, 1)).set('x', (0, 2))


  @scala.annotation.tailrec
  def play(boards: Seq[Board], piece: Char): Seq[Board] = {
    if (boards.last.done) boards
    else {
      val curBoard = boards.last
      play(boards :+ curBoard.set(piece, curBoard.bestMove(piece)), nextPiece(piece))
    }
  }

//  def allPossibleGames(board: Board, piece: Char): Seq[Seq[Board]] = {
//    for (free <- board.frees)
//  }

  val result = for {
    init <- Seq((2, 0), (1, 1), (2, 1), (1, 2), (2, 2))
  } yield init -> play(Seq(initial.set('x', init)), 'o')

  result.foreach(println)
  result.map(x => x._1 -> x._2.map(_.win)).foreach(println)
}


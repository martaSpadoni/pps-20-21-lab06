package u06lab.code

import u06lab.code.TicTacToe.{Board, Mark, O, X, computeAnyGame, find, placeAnyMark, printBoards}

import u06lab.code.TicTacToe.HasWon._

object TicTacToe {

  sealed trait Player {
    def other: Player = this match {
      case X => O;
      case _ => X
    }

    override def toString: String = this match {
      case X => "X";
      case _ => "O"
    }
  }

  case object X extends Player

  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)

  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(m => m.x == x && m.y == y).map(_.player)

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    for {
      x <- 0 to 2
      y <- 0 to 2
      mark = Mark(x, y, player)
      if find(board, x, y).isEmpty
    } yield mark :: board
  }

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream(List(Nil))
    case _ => (for{
      g <- computeAnyGame(player.other,moves-1)
      b <- placeAnyMark(g.head, player)
    } yield if(someoneHasWon(g.head)) g else  b :: g).distinct
  }

  object HasWon{

    def someoneHasWonIn(board: Board, in:(Int, Int) => Boolean):Boolean = board.collect{
        case Mark(i, j, X) if in(i,j) => 1
        case Mark(i, j, O) if in(i,j) => -1
      }.sum.abs == 3

    def someoneHasWonInRow(board: Board, x:Int): Boolean = someoneHasWonIn(board, (i,_) => i == x)
    def someoneHasWonInCol(board: Board, y:Int): Boolean = someoneHasWonIn(board, (_,j) => j == y)
    def someoneHasWonInDiagonal(board: Board):Boolean = someoneHasWonIn(board, (i,j) => i==j) ||
                                                        someoneHasWonIn(board, (i,j) => i+j == 2)
    def someoneHasWon(board: Board):Boolean = (for {
      i <- 0 to 2
    } yield someoneHasWonInRow(board, i) || someoneHasWonInCol(board, i)).contains(true) || someoneHasWonInDiagonal(board)
  }

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) {
        print(" "); if (board == game.head) println()
      }
    }
}

object TicTacToeTest extends App {
  // Exercise 1: implement find such that..
  println(find(List(Mark(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  println()
//  val b:Board = List(Mark(2, 0,O), Mark(1, 1,O), Mark(0, 2,O))
//  printBoards(Seq(b))
//  println(someoneHasWon(b))
  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  //println(computeAnyGame(O, 6).size)
  computeAnyGame(O, 6) foreach {g => printBoards(g); println()}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}



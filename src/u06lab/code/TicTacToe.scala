package u06lab.code

import u06lab.code.TicTacToe.{Board, Mark, O, X, computeAnyGame, find, placeAnyMark, printBoards}

import java.util.function.Predicate

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

  object HasWon{

    def row(x:Int):PartialFunction[Mark, Int] = fun((i,_) => i == x)

    def col(y:Int):PartialFunction[Mark, Int] = fun((_,j) => j == y)

    def diag1():PartialFunction[Mark, Int] = fun((i,j) => i-j == 0)

    def diag2():PartialFunction[Mark, Int] = fun((i,j) => i+j == 2)

    def fun(pred: (Int, Int) => Boolean):PartialFunction[Mark, Int] = {
      case Mark(i, j, X) if pred(i,j) => 1
      case Mark(i, j, O) if pred(i,j) => -1
    }

    def someoneHasWonIn(board: Board, in:PartialFunction[Mark, Int]):Boolean = {
      board.collect(in).sum.abs == 3
    }

    def someoneHasWon(board: Board):Boolean = {
      var hasWon:Boolean = false;
      for(x <- 0 to 2 if !hasWon){
        hasWon = someoneHasWonIn(board, row(x)) ||
              someoneHasWonIn(board, col(x)) ||
              someoneHasWonIn(board, diag1())||
              someoneHasWonIn(board, diag2())
      }
      hasWon
    }
  }



  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream(List(Nil))
    case _ => for{
      g <- computeAnyGame(player.other,moves-1)
      b <- placeAnyMark(g.head, player)
    } yield if(HasWon.someoneHasWon(g.head)) g else b :: g
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



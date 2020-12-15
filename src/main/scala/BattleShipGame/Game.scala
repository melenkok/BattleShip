package BattleshipGame
import BattleshipGame.Board.Board

class Game (val board: Board, val ships: (List[(Int, Int)], List[(Int, Int)], List[(Int, Int)],
  List[(Int, Int)], List[(Int, Int)] )) {

  val WinglerShip = ships._1
  val AngleShip = ships._2
  val AclassShip = ships._3
  val BclassShip = ships._4
  val SclassShip = ships._5

  def placeShots( shots: List[(Int, Int)]): Game = {

    shots.foreach { case (x, y) => {
      if (board.board(x)(y) == Board.charEmpty ) board.board(x)(y) = Board.missedShot
      else {board.board(x)(y) = Board.shipHit}
    }}
    new Game(board, this.ships)
  }

}

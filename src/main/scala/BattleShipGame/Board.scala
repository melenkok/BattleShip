package BattleshipGame
import BattleshipGame.Board.{Board, RandomNumber}
import BattleshipGame.Ships
import BattleshipGame.Game

import scala.util.Random

object Board {

  val boardLength: Int = 16
  val charEmpty: Char = '.'
  val missedShot: Char = '-'

  val shipHit: Char = 'X'
  val ship: Char = '*'



  // makes an empty board with just (water)
  def make_board(): Array[Array[Char]] = {
    val board: Array[Array[Char]] = Array.ofDim[Char](boardLength, boardLength)
    for (i: Int <- 0 to board.length - 1; j: Int <- 0 to board.length - 1) {
      board(i)(j) = charEmpty
    }
    board
  }

  def fill_board():  Game = {

    var firstShip = new Board(Board.make_board()).place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Wingler_ship)
    var secondShip = firstShip._1.place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Angle_ship)
    var thirdShip = secondShip._1.place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Aclass_ship)
    var fourthShip = thirdShip._1.place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Bclass_ship)
    var fifthShip = fourthShip._1.place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Sclass_ship)

    val WinglerShip = firstShip._2
    val AngleShip = secondShip._2
    val AclassShip = thirdShip._2
    val BclassShip = fourthShip._2
    val SclassShip = fifthShip._2

    return new Game(fifthShip._1, (WinglerShip, AngleShip, AclassShip, BclassShip, SclassShip))


    //new Board(Board.make_board()).place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Wingler_ship)._1
    //val WinglerShip =
    //      .place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Angle_ship)
    //      .place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Aclass_ship)
    //      .place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Bclass_ship)
    //      .place_ship(new Ships(new RandomNumber().x, new RandomNumber().y).Sclass_ship)
  }

  // prints boards
  def printer(board: Array[Array[Char]]): Unit = {
    for (i <- 0 to 15) {
      var line: String = ""
      for (j <- 0 to 15) {
        line += board(i)(j)
      }
      println(line)
    }
  }

  class Board(val board: Array[Array[Char]]) {

    def printer: Unit = {
      for (i <- 0 to 15) {
        var line: String = ""
        for (j <- 0 to 15) {
          line += board(i)(j)
        }
        println(line)
      }
    }

    def place_ship(coordinates: => List[(Int, Int)] ): (Board, List[(Int, Int)] )= {
      var temp_coordinates= coordinates
      if (temp_coordinates.forall{case(x,y) => x>=0 && y>=0 && x<16 && y<16 && board(x)(y) != Board.ship})
      //coordinates.forall{case(x,y) => board(x)(y) != "*"})
      {
        temp_coordinates.foreach { case (x, y) => {
          board(x)(y) = Board.ship}}
        //new Board(board)
        val tuple_result = (new Board(board), temp_coordinates)
        tuple_result
      }
      else place_ship(coordinates)
    }
  }


  class RandomNumber(x_offset:Int = 2, y_offset:Int = 1){
    val x:Int = new Random().nextInt(boardLength)//shuffle(0 to GameBase.boardLength - 5).toString().toInt
    val y:Int = new Random().nextInt(boardLength)//shuffle(0 to GameBase.boardLength - 3).toString().toInt
  }
}



object printaj extends App{


  val finalboard = Board.fill_board()

  finalboard.board.printer
  println(s"Wingler_ship is on: ${finalboard.WinglerShip}")
  println(s"AngleShip is on: ${finalboard.AngleShip}")
  println(s"AclassShip is on: ${finalboard.AclassShip}")
  println(s"BclassShip is on ${finalboard.BclassShip}")
  println(s"SclassShip is on ${finalboard.SclassShip}")

  val prvipotez = finalboard.placeShots( List( (4,5), (10,7), (8,4)) )

  prvipotez.board.printer
}

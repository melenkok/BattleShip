package BattleshipGame

import scala.util.Random

class Ships(x:Int,  y:Int) {

  val angleOptions: Array[Double] = Array(0, 90.0, 180.0, 270.0)
  val angle: Double = Random.shuffle(angleOptions.toList).head


  def transpose_x(x: Int, y: Int, centreX: Int, centreY: Int, angle: => Double): Int = {
    def transposed_x = ((x - centreX) * math.cos(angle) + centreX) - ((y - centreY) * math.sin(angle) + centreY)

    return (transposed_x.round.toInt)
  }

  def transpose_y(y: Int, x: Int, centreY: Int, centreX: Int, angle:  => Double): Int = {
    val transposed_y = ((x - centreX) * math.sin(angle) + centreX) + ((y - centreY) * math.cos(angle) + centreY)
    return (transposed_y.round.toInt)
  }


  def Wingler_ship: List[(Int, Int)] = {
    val coordinates = List((x-2, y-1), (x-2, y+1), (x-1, y-1),
      (x-1, y+1), (x, y), (x+1, y-1), (x+1, y+1), (x+2, y-1), (x+2, y+1))
    val finalCoordinates = coordinates.map{ case(x,y) =>  (transpose_x( x,  y , this x, this y, angle*(math.Pi /180.0)),
      transpose_y(y, x, this x, this y, angle *(math.Pi/180.0))) }
    finalCoordinates
  }

  def Angle_ship : List[(Int, Int)] = {
    val coordinates = List( (x,y), (x-1,y), (x-2, y), (x-3, y), (x,y+1), (x, y+2))
    val finalCoordinates = coordinates.map{ case(x,y) =>  (transpose_x( x,  y , this x, this y, angle*(math.Pi /180.0)),
      transpose_y(y, x, this x, this y, angle *(math.Pi/180.0))) }
    finalCoordinates
  }

  def Aclass_ship : List[(Int, Int)] = {
    val coordinates = List( (x -2,y), (x-1,y-1), (x-1, y+1), (x, y-1), (x,y), (x, y+1),
      (x+1, y-1), (x+1, y+1))
    val finalCoordinates = coordinates.map{ case(x,y) =>  (transpose_x( x,  y , this x, this y, angle*(math.Pi /180.0)),
      transpose_y(y, x, this x, this y, angle *(math.Pi/180.0))) }
    finalCoordinates
  }

  def Bclass_ship : List[(Int, Int)] = {
    val coordinates = List( (x -2,y-1), (x-2,y), (x-1, y-1),(x-1, y+1), (x, y-1), (x,y), (x+1, y-1),
      (x+1, y+1), (x+2, y-1), (x+2, y))
    coordinates
  }

  def Sclass_ship : List[(Int, Int)] = {
    val coordinates = List( (x -2,y), (x-2,y+1), (x-1, y-1),(x, y), (x, y+1), (x+1,y+1), (x+2, y-1),
      (x+2, y))
    coordinates
  }

  val ships = Map(
    "Wingler" -> {Wingler_ship},
    "Angle_ship" -> {Angle_ship}
  )

}

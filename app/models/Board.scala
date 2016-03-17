package models

class Board(size: Int) {
  
  val playfield = Array.ofDim[Char](size,size)
  
  def placeStone(c: Char, x:Int, y:Int) {
    if ( playfield(x)(y) == 'w' | playfield(x)(y) == 'b' ) {
      println("ER: " + c + " not placed on " + x + "," + y)
    } else {
      println("OK: " + c + " placed on " + x + "," + y)
      playfield(x)(y) = c
    }
  }
  
  def placeManyStones(c: Char, positions: List[Point]) {
    for( pos <- positions ) {
      placeStone(c, pos.x, pos.y)
    }
  }
  
  def getColor(x: Int, y: Int) = {
    playfield(x)(y)
  }
  
}
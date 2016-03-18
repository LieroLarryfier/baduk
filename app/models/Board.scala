package models

class Board(size: Int) {
  
  val playfield = Array.ofDim[Char](size,size)
  
  def placeStone(c: Char, x:Int, y:Int) {
    if ( playfield(x)(y) == 0 ) {
      playfield(x)(y) = c
      println("OK: " + c + " placed on " + x + "," + y)
    } else {
       println("ER: " + c + " not placed on " + x + "," + y + " (not empty)")
    }
  }
  
  def placeManyStones(c: Char, positions: List[Point]) {
    for( pos <- positions ) {
      playfield(pos.x)(pos.y) = c
    }
  }
  
  def getColor(x: Int, y: Int) : Char = {
    playfield(x)(y)
  }
  
  def getNeighbours(point: Point) : Array[Point] = {
    val neighbours = new Array[Point](4)
    val px = point.x;
    val py = point.y;
    
    if ((px - 1) < 0) {
      neighbours(0) = null
    } else {
      neighbours(0) = new Point(px-1,py)
    }
    
    if ((px + 1) > playfield(0).length) {
      neighbours(1) = null
    } else {
      neighbours(1) = new Point(px+1,py)
    }
    
    if ((py - 1) < 0) {
      neighbours(2) = null
    } else {
      neighbours(2) = new Point(px,py-1)
    }
    
    if ((py + 1) > playfield(0).length) {
      neighbours(3) = null
    } else {
      neighbours(3) = new Point(px,py+1)
    }
    
    neighbours.filter(_ != null)
    
  }
  
}
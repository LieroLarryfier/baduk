package models

class Board(size: Int) {
  
  val playfield = Array.ofDim[Char](size,size)
  
  def placeStone(c: Char, x: Int, y: Int) : String = {
    placeStone(c, new Point(x,y))
  }
  
  def placeStone(c: Char, p: Point) : String = {
    val x = p.x
    val y = p.y
    if ( playfield(x)(y) == 0 ) {
      
      var neighbours = getNeighbours(new Point(x,y))
      var neighbourColors = new Array[Char](4)
      var i = 0
     
      if (!neighbours.isEmpty) {
        for ( a <- neighbours ) {
          
          neighbourColors(i) = getColor(a)
          i = i + 1
          }
        }
      if (neighbourColors.forall(b => b == c) || neighbourColors.exists(b => b == 0)) {
          
        playfield(x)(y) = c
        "OK: " + c + " placed on " + x + "," + y
      } else {
        "ER: " + c + " not placed on " + x + "," + y + " (no liberties)"
      }
     
    } else {
       "ER: " + c + " not placed on " + x + "," + y + " (not empty)"
    }
  }
  
  def checkLiberties(col: Char, p: Point) : Boolean = {
    println("Start:" + p.x + "," + p.y)
    val visited = List[Point]()
    p :: visited
    for (n <- getNeighbours(p)) {
      var nCol = getColor(n)
      if (nCol == col && !visited.contains(n)) {
        checkLiberties(col, n)
        println(n.x + "," + n.y)
      }
    }
    
    true
  }
  
  def placeManyStones(c: Char, positions: List[Point]) {
    for( pos <- positions ) {
      playfield(pos.x)(pos.y) = c
    }
  }
  
  def getColor(x: Int, y: Int) : Char = {
    playfield(x)(y)
  }
  
  def getColor(point: Point) : Char = {
    getColor(point.x, point.y)
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
    
    if ((px + 1) >= playfield(0).length) {
      neighbours(1) = null
    } else {
      neighbours(1) = new Point(px+1,py)
    }
    
    if ((py - 1) < 0) {
      neighbours(2) = null
    } else {
      neighbours(2) = new Point(px,py-1)
    }
    
    if ((py + 1) >= playfield(0).length) {
      neighbours(3) = null
    } else {
      neighbours(3) = new Point(px,py+1)
    }
    
    neighbours.filter(_ != null)
    
  }
  
}
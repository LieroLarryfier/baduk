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
      var neighbourColors = new Array[Char](neighbours.length)
      var i = 0
     
      if (!neighbours.isEmpty) {
        for ( a <- neighbours ) {
          
          neighbourColors(i) = getColor(a)
          i = i + 1
          }
      }
      if (neighbourColors.forall(b => b == c) || neighbourColors.exists(b => b == 0)) {
          
        playfield(x)(y) = c
        
        if (x > 0)     maybeCapture(new Point(x-1,y));
	      if (y > 0)     maybeCapture(new Point(x,y-1));
	      if (x < size-1) maybeCapture(new Point(x+1,y));
	      if (y < size-1) maybeCapture(new Point(x,y+1));
	      "OK: " + c + " placed on " + x + "," + y
      } else {
        "ER: " + c + " not placed on " + x + "," + y + " (no liberties)"
      }
     
    } else {
       "ER: " + c + " not placed on " + x + "," + y + " (not empty)"
    }
  }
  
  def maybeCapture(p: Point) : Int = {
    val x = p.x;
    val y = p.y;
  if (playfield(x)(y) != 0) {
    var alive = hasLiberties(playfield(x)(y), p, playfield.map(_.clone));
    if (!alive)
      return removeStones(playfield(x)(y),p);
  }
  return 0;
}
  
  def hasLiberties(col: Char, p: Point, playfieldCopy: Array[Array[Char]]) : Boolean = {
    val x = p.x
    val y = p.y
    if (playfieldCopy(x)(y) == 0)
      return true;	
    else if (playfieldCopy(x)(y) != col)
      return false;
    else {
      playfieldCopy(x)(y) = 's'; /* avoid looping */
      if (x > 0     && hasLiberties(col,new Point(x-1,y),playfieldCopy)) return true;
      if (y > 0     && hasLiberties(col,new Point(x,y-1),playfieldCopy)) return true;
      if (x < size-1 && hasLiberties(col,new Point(x+1,y),playfieldCopy)) return true;
      if (y < size-1 && hasLiberties(col,new Point(x,y+1),playfieldCopy)) return true;
      return false;
    }
  }
  
  def removeStones(col: Char, p: Point) : Int = {
    val x = p.x
    val y = p.y
    
    if (playfield(x)(y) != col)
    return 0;
  else {
    var count = 1;
    playfield(x)(y) = 0;
    if (x > 0)     count += removeStones(col,new Point(x-1,y));
    if (y > 0)     count += removeStones(col,new Point(x,y-1));
    if (x < size-1) count += removeStones(col,new Point(x+1,y));
    if (y < size-1) count += removeStones(col, new Point(x,y+1));
    return count;
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
    
    if ((px + 1) > size-1) {
      neighbours(1) = null
    } else {
      neighbours(1) = new Point(px+1,py)
    }
    
    if ((py - 1) < 0) {
      neighbours(2) = null
    } else {
      neighbours(2) = new Point(px,py-1)
    }
    
    if ((py + 1) > size-1) {
      neighbours(3) = null
    } else {
      neighbours(3) = new Point(px,py+1)
    }
    
    neighbours.filter(_ != null)
    
  }
  
  def printBoard {
    println(playfield.deep.mkString("\n"))
  }
  
}
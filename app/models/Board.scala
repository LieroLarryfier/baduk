package models

class Board(size: Int) {
  
  var playfield = Array.ofDim[Char](size,size)
  
  def placeStone(c: Char, x: Int, y: Int) : String = {
    placeStone(c, new Point(x,y))
  }
  
  def placeStone(c: Char, p: Point) : String = {
    val x = p.x
    val y = p.y
    if ( playfield(x)(y) == 0 ) {
      
      
        var checkBoard = playfield.map(_.clone)
        checkBoard(x)(y) = c
	      if (!hasLiberties(c, p, checkBoard)) {
	        return "ER: " + c + " not placed on " + x + "," + y + " (no liberties)"
	        
	      } else {
        val newBoard = playfield.map(_.clone)
        newBoard(x)(y) = c
        
        if (x > 0)     maybeCapture(newBoard, new Point(x-1,y));
	      if (y > 0)     maybeCapture(newBoard, new Point(x,y-1));
	      if (x < size-1) maybeCapture(newBoard, new Point(x+1,y));
	      if (y < size-1) maybeCapture(newBoard, new Point(x,y+1));
	      
	      
	        playfield = newBoard
	        "OK: " + c + " placed on " + x + "," + y  
	      }   
    } else {
       "ER: " + c + " not placed on " + x + "," + y + " (not empty)"
    }
  }
  
  def maybeCapture(newBoard: Array[Array[Char]], p: Point) : Int = {
    val x = p.x;
    val y = p.y;
  if (newBoard(x)(y) != 0) {
    var checkBoard = newBoard.map(_.clone)
    var alive = hasLiberties(newBoard(x)(y), p, checkBoard);
    if (!alive)
      return removeStones(newBoard(x)(y),p, newBoard);
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
  
  def removeStones(col: Char, p: Point, newBoard: Array[Array[Char]]) : Int = {
    val x = p.x
    val y = p.y
    
    if (newBoard(x)(y) != col) {
     0
    }
  else {
    var count = 1;
    newBoard(x)(y) = 0;
    if (x > 0)     count += removeStones(col,new Point(x-1,y), newBoard);
    if (y > 0)     count += removeStones(col,new Point(x,y-1), newBoard);
    if (x < size-1) count += removeStones(col,new Point(x+1,y), newBoard);
    if (y < size-1) count += removeStones(col, new Point(x,y+1), newBoard);
    count
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
    print(playfield.map(_.mkString("|")).mkString("|\n|"))
  }
  
}
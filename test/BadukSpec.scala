import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import models.Board
import models.Point
import exceptions.NoLegalMoveException

class BadukSpec extends PlaySpec {
  
  
  "A stone" when { 
    val board = new Board(5)
    "in the middle of the board" must {
      "have 4 neighours" in {
       board.getNeighbours(new Point(2,2)).length mustBe 4
      }
    }
    "on the top edge of the board" must {
    "have 3 neighours" in {
      board.getNeighbours(new Point(0,2)).length mustBe 3
      }
    }
    "on the right edge of the board" must {
    "have 3 neighours" in {
      board.getNeighbours(new Point(2,4)).length mustBe 3
      }
    }
    "in the top left corner of the board" must {
    "have 2 neighours" in {
      board.getNeighbours(new Point(0,0)).length mustBe 2
      }
    }
    "in the bottom right corner of the board" must {
    "have 2 neighours" in {
      board.getNeighbours(new Point(4,4)).length mustBe 2
      }
    }
  }
  
  
  "Placing many stones on board" must {
    "contain the stones" in {
      val board = new Board(6)
      board.placeManyStones('w', List(new Point(2,3), new Point(3,2), new Point(4,3), new Point(3,4)))
      board.getColor(2, 3) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(4, 3) mustBe ('w')
      board.getColor(3, 4) mustBe ('w')
    }    
  }
    
  
  "Place stone" when {
    
    "board is empty" in {
      val board = new Board(5)
      board.placeStone('w', 2, 2)
      board.getColor(2, 2) mustBe ('w')
    }    
    
    "legal move" in {
      val board = new Board(5)
      board.placeStone('w', new Point(2, 2))
      board.placeStone('b', new Point(2, 3))
      board.getColor(2, 2) mustBe ('w')
      board.getColor(2, 3) mustBe ('b')
    }    
    
    "in a hole" in {
      val board = new Board(5)
      board.placeManyStones('b', List(new Point(1,2), new Point(1,3), new Point(2,1), new Point(3,2), new Point(3,3)))
      board.placeManyStones('w', List(new Point(1,4), new Point(2,3), new Point(2,4), new Point(3,4)))
      board.placeStone('w', new Point(2,2)) must include ("OK")
      board.getColor(1, 2) mustBe ('b')
      board.getColor(1, 3) mustBe ('b')
      board.getColor(2, 1) mustBe ('b')
      board.getColor(3, 2) mustBe ('b')
      board.getColor(3, 3) mustBe ('b')
      board.getColor(1, 4) mustBe ('w')
      board.getColor(2, 3) mustBe ('w')
      board.getColor(2, 4) mustBe ('w')
      board.getColor(3, 4) mustBe ('w')
      board.getColor(2, 2) must be ('w')
    }
  }
  
  
  "Don't place stone" when {
    
    "point is not empty" in {
      val board = new Board(5)
      board.placeStone('w', 2, 2)
      board.placeStone('b', 2, 2) must include ("not empty")
      board.getColor(2, 2) mustBe ('w')
    } 
    
    "no liberties" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,2), new Point(2,1), new Point(3,2), new Point(2,3)))
      board.placeStone('b', 2, 2) must include ("no liberties")
      board.getColor(1, 2) mustBe ('w')
      board.getColor(2, 1) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(2, 3) mustBe ('w')
      board.getColor(2, 2) must be (0)
    }
    
    "no liberties in the corner" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,0), new Point(0,1)))
      board.placeStone('b', 0, 0) must include ("no liberties")
      board.getColor(1, 0) mustBe ('w')
      board.getColor(0, 1) mustBe ('w')
      board.getColor(0, 0) must be (0)
    }
    
    "no liberties, even when to own stone" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,2), new Point(1,3), new Point(2,1), new Point(2,4), new Point(3,2), new Point(3,3)))
      board.placeManyStones('b', List(new Point(2,3)))
      board.printBoard
      board.placeStone('b', 2, 2) must include ("no liberties")
      board.getColor(1, 2) mustBe ('w')
      board.getColor(1, 3) mustBe ('w')
      board.getColor(2, 1) mustBe ('w')
      board.getColor(2, 4) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(3, 3) mustBe ('w')
      board.getColor(2, 2) must be (0)
      board.getColor(2, 3) must be ('b')
    }   
    
  }
    
  "Capture a stone" when {
    
    "in the middle of the board" in {
      
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,2), new Point(2,1), new Point(3,2)))
      board.placeManyStones('b', List(new Point(2,2)))
      board.placeStone('w', new Point(2,3)) must include ("OK")
      board.getColor(1, 2) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(2, 3) mustBe ('w')
      board.getColor(2, 1) mustBe ('w')
      board.getColor(2, 2) must be (0)
    }
    
    "in the corner of the board" in {
    
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(3,4)))
      board.placeManyStones('b', List(new Point(4,4)))
      board.placeStone('w', new Point(4,3)) must include ("OK")
      board.getColor(4, 3) mustBe ('w')
      board.getColor(3, 4) mustBe ('w')
      board.getColor(4, 4) must be (0)
    }
  }
  
  "Capture stones" when {
    
    "in the middle of the board" in {
    
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(2,2), new Point(2,3), new Point(3,3)))
      board.placeManyStones('b', List(new Point(1,2), new Point(1,3), new Point(2,4), new Point(3,2), new Point(3,4), new Point(4,3)))
      board.placeStone('b', new Point(2,1)) must include ("OK")
      board.getColor(1, 2) mustBe ('b')
      board.getColor(1, 3) mustBe ('b')
      board.getColor(2, 4) mustBe ('b')
      board.getColor(3, 2) mustBe ('b')
      board.getColor(3, 4) mustBe ('b')
      board.getColor(4, 3) mustBe ('b')
      board.getColor(2, 1) mustBe ('b')
      board.getColor(2, 3) mustBe (0)
      board.getColor(3, 3) mustBe (0)
      board.getColor(2, 2) must be (0)
      
    }
    
    "on the edge of the board" in {
      
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(0,4), new Point(1,4), new Point(2,4), new Point(3,4), new Point(4,4)))
      board.placeManyStones('b', List(new Point(1,3), new Point(2,3), new Point(3,3), new Point(4,3)))
      board.placeStone('b', new Point(0,3)) must include ("OK")
      board.getColor(0, 3) must be ('b')
      board.getColor(1, 3) must be ('b')
      board.getColor(2, 3) must be ('b')
      board.getColor(3, 3) must be ('b')
      board.getColor(4, 3) must be ('b')
      board.getColor(0, 4) must be (0)
      board.getColor(1, 4) must be (0)
      board.getColor(2, 4) must be (0)
      board.getColor(3, 4) must be (0)
      board.getColor(4, 4) must be (0)
    }
  }
  
  
  
  "MaybeCapture" should {
    "return number of captured stones" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,2), new Point(2,1), new Point(3,2), new Point(2,3)))
      board.placeManyStones('b', List(new Point(2,2)))
      board.maybeCapture(board.playfield, new Point(2,2)) must be (1)
    }
  }
  
  "A stone" must {
    "have liberties" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,2), new Point(1,1), new Point(2,1)))
      board.hasLiberties('w', new Point(1,1), board.playfield.clone()) must be (true)
    }
  }
  
  "A group" must {
    "be removed" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,2), new Point(1,1), new Point(2,1), new Point(3,3)))
      board.removeStones('w', new Point(1,1), board.playfield.clone()) must be (3)
    }
  }
  
}
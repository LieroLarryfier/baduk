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
  
  
  "When placing many stones on board, board" must {
    "contain the stones" in {
      val board = new Board(6)
      board.placeManyStones('w', List(new Point(2,3), new Point(3,2), new Point(4,3), new Point(3,4)))
      board.getColor(2, 3) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(4, 3) mustBe ('w')
      board.getColor(3, 4) mustBe ('w')
    }    
  }
  
  "After placing a stone, the board" must {
    "contain the stone" in {
      val board = new Board(6)
      board.placeStone('w', 3, 3)
      board.getColor(3, 3) mustBe ('w')
    }    
  }
  
  "Placing a stone over another stone, board" must {
      val board = new Board(6)
      board.placeStone('w', 3, 3)
    "contain only the other stone" in {
      
      board.placeStone('b', 3, 3) must include ("not empty")
      board.getColor(3, 3) mustBe ('w')
    }    
  }
  
  "When placing a next to another stone, board" must {
    "contain the other stone" in {
      val board = new Board(6)
      board.placeStone('w', new Point(3, 3))
      board.placeStone('b', new Point(3, 4))
      board.getColor(3, 4) mustBe ('b')
    }    
  }
  
  
  "When placing a stone to no liberties, board" must {
    "contain only the other stones" in {
      val board = new Board(6)
      board.placeManyStones('w', List(new Point(2,3), new Point(3,2), new Point(4,3), new Point(3,4)))
      board.placeStone('b', 3, 3) must include ("no liberties")
      board.getColor(2, 3) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(4, 3) mustBe ('w')
      board.getColor(3, 4) mustBe ('w')
      board.getColor(3, 3) must not be ('b')
    }    
  }
  
  "A Stone" should {
    "be captured when no liberties" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,2), new Point(2,1), new Point(3,2)))
      board.placeStone('b', new Point(2,2))
      board.placeStone('w', new Point(2,3))
      board.getColor(1, 2) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(2, 3) mustBe ('w')
      board.getColor(2, 1) mustBe ('w')
      board.getColor(2, 2) must be (0)
    }
    
  }
  
  "check Liberties" must {
    "terminate" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(1,2), new Point(1,1), new Point(2,1)))
      board.checkLiberties('w', new Point(1,1))
    }
  }
  
  
}
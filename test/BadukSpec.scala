import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import models.Board
import models.Point

class BadukSpec extends PlaySpec {
  
  "After placing a stone, the board" must {
    "contain the stone" in {
      val board = new Board(5)
      board.placeStone('w', 3, 3)
      board.getColor(3, 3) mustBe ('w')
    }    
  }
  
  "Placing a stone over another stone, board" must {
    "contain only the other stone" in {
      val board = new Board(5)
      board.placeStone('w', 3, 3)
      board.placeStone('b', 3, 3)
      board.getColor(3, 3) mustBe ('w')
    }    
  }
  
  "When placing a next to another stone, board" must {
    "contains the other stone" in {
      val board = new Board(5)
      board.placeStone('w', 3, 3)
      board.placeStone('b', 3, 4)
      board.getColor(3, 4) mustBe ('b')
    }    
  }
  
  "When placing many stones, board" must {
    "contains the stones" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(2,3), new Point(3,2), new Point(4,3), new Point(3,4)))
      board.getColor(2, 3) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(4, 3) mustBe ('w')
      board.getColor(3, 4) mustBe ('w')
    }    
  }
  
  "When placing a stone to no liberties, board" must {
    "contains only the other stones" in {
      val board = new Board(5)
      board.placeManyStones('w', List(new Point(2,3), new Point(3,2), new Point(4,3), new Point(3,4)))
      board.placeStone('b', 3, 3)
      board.getColor(2, 3) mustBe ('w')
      board.getColor(3, 2) mustBe ('w')
      board.getColor(4, 3) mustBe ('w')
      board.getColor(3, 4) mustBe ('w')
      board.getColor(3, 3) must not be ('b')
    }    
  }
}
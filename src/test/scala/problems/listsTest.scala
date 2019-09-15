package problems

import org.scalatest._
import Matchers._
import problems.lists._

class listsTest extends FunSpec with Matchers {

  describe("The P01"){
    describe("with the simplest method"){
      it("Should return the last element of a list :: Integer"){
      val input = List(1,2,6,99)
      val result = lists .last_element_builtin(input)

      result should be (99)

      }
      it("Should return the last element of a list :: String"){
        val input = List("Alpha","Beta")
        val result = lists .last_element_builtin(input)

        result should be ("Beta")
      }
    }
    describe("using the recursive tail approach"){
      it("Should return the last element of a list :: Integer"){
        val input = List(1,2,6,99)
        val result = last_element_func(input)

        result should be (99)

        }
      it("Should return the last element of a list :: String"){
        val input = List("Alpha","Beta")
        val result = last_element_func(input)

        result should be ("Beta")

        }
    }
  }
  describe("The P02"){
    describe("with the built-in solution"){
      it("should return the penultime element of a list :: Integer"){
        val input = List(0,1,3,5,7)
        val result = lists .penultime_builtin(input)

        result should be (5)

      }
      it("should return the penultime element of a list :: String"){
        val input = List("one","three","last")
        val result = penultime(input)

        result should be ("three")

      }
    }
    describe("with the recursive approach"){
      it("should return the penultime element of a list :: Integer"){
        val input = List(0,5,3,19,8)
        val result = penultime(input)

        result should be (19)

      }
      it("should return the penultime element of a list :: String"){
        val input = List("one","penultime","last")
        val result = penultime(input)

        result should be ("penultime")
      }
      it("should return a NoSuchElementException with a null list"){
        val input = List()
        an [NoSuchElementException] should be thrownBy {
          penultime(input)
        }
      }
    }
  }


}

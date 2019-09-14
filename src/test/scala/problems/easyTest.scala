package problems

import org.scalatest._
import Matchers._
import problems.easy._

class easyTest extends FunSpec with Matchers {

  describe("The P01"){
    describe("with the simplest method"){
      it("Should return the last element of a list :: Integers"){
      val input = List(1,2,6,99)
      val result = easy .last_element_simplest(input)

      result should be (99)

    }
      it("Should return the last element of a list :: String"){
        val input = List("Alpha","Beta")
        val result = easy .last_element_simplest(input)

        result should be ("Beta")
      }
    }
  describe("using the recursive tail approach"){
    it("Should return the last element of a list :: Integers"){
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


}

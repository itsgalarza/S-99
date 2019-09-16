package problems

import org.scalatest._
import Matchers._
import problems.lists._

class listsTest extends FunSpec with Matchers {

  describe("The P01"){
    describe("with the built-in method"){
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
        val result = lists .penultimate_builtin(input)

        result should be (5)

      }
      it("should return the penultime element of a list :: String"){
        val input = List("one","three","last")
        val result = penultimate(input)

        result should be ("three")

      }
    }
    describe("with the recursive approach"){
      it("should return the penultime element of a list :: Integer"){
        val input = List(0,5,3,19,8)
        val result = penultimate(input)

        result should be (19)

      }
      it("should return the penultime element of a list :: String"){
        val input = List("one","penultime","last")
        val result = penultimate(input)

        result should be ("penultime")
      }
      it("should return a NoSuchElementException with a null list"){
        val input = List()
        an [NoSuchElementException] should be thrownBy {
          penultimate(input)
        }
      }
    }
  }
  describe("The P03"){
    describe("using a counter"){
      it("should return the nth element of a list :: Integer"){
        val input = List(1,2,5,8,9,0)
        val result = nth_counter(4, input)

        result should be (8)

      }
      it("should return the nth element of a list :: String"){
        val input = List("hi","bye","this")
        val result = nth_counter(3, input)

        result should be ("this")

      }
      it("should return NoSuchElementException if the nth element is not in the list"){
        val input = List(1,4,5)

        an [NoSuchElementException] should be thrownBy {
          nth_counter(5,input)
        }
      }
    }
    describe("using the simplest recursive method"){
      it("should return the nth element of a list :: Integer"){
        val input = List(1,2,5,8,9,0)
        val result = nth_recursive(4, input)

        result should be (8)

      }
      it("should return the nth element of a list :: String"){
        val input = List("hi","bye","this")
        val result = nth_recursive(3, input)

        result should be ("this")

      }
      it("should return NoSuchElementException if the nth element is not in the list"){
        val input = List(1,4,5)

        an [NoSuchElementException] should be thrownBy {
          nth_recursive(5,input)
        }
      }
    }
    describe("using the built-in method"){
      it("should return the nth element of a list :: Integer"){
        val input = List(1,2,5,8,9,0)
        val result = nth_builtin(4, input)

        result should be (8)

      }
      it("should return the nth element of a list :: String"){
        val input = List("hi","bye","this")
        val result = nth_builtin(3, input)

        result should be ("this")

      }
      it("should return NoSuchElementException if the nth element is not in the list"){
        val input = List(1,4,5)

        an [NoSuchElementException] should be thrownBy {
          nth_builtin(5,input)
        }
      }
    }
  }
  describe("The P04"){
    describe("using the built-in method"){
      it("should return the size of the input list"){
        val input = List(0,2,3,4,4,5)
        val result = length_builtin(input)

        result should be (6)
      }
      it("should return 0 when the list is empty"){
        val input = List()
        val result = length_builtin(input)

        result should be (0)

      }
    }
    describe("using recursive approach"){
      it("should return the size of the input list :: List has values"){
        val input = List(1,2,3,4,5)
        val result = length_recursive(input)

        result should be (5)

      }
      it("should return the size of the input list == 0 :: List is empty"){
        val input = List()
        val result = length_recursive(input)

        result should be (0)

      }
    }
    describe("using the more pure functional approach"){
      it("should return the size of the input list :: List has values"){
        val input = List(1,2,3,4,5)
        val result = length_functional(input)

        result should be (5)
      }
      it("should return the size of the input list == 0 :: List is empty"){
        val input = List()
        val result = length_functional(input)

        result should be (0)

      }
    }
  }
  describe("The P05"){
    describe("using the built-in method"){
      it("should return the reversed input list"){
        val input = List(1,2,3,4)
        val result = reverse(input)

        result should be (List(4,3,2,1))
      }
      it("should return the same list if the input is an empty list"){
        val input = List()
        val result = reverse(input)

        result should be (input)
      }
    }
    describe("using functional approach"){
      it("should return the reversed input list"){
        val input = List(1,2,3,4)
        val result = reverse_func(input)

        result should be (List(4,3,2,1))
      }
      it("should return the same list if the input is an empty list"){
        val input = List()
        val result = reverse_func(input)

        result should be (input)
      }
    }
    describe("using a more pure functional approach"){
      it("should return the reversed input list"){
        val input = List(1,2,3,4)
        val result = reverse_pure(input)

        result should be (List(4,3,2,1))
      }
      it("should return the same list if the input is an empty list"){
        val input = List()
        val result = reverse_pure(input)

        result should be (input)
      }
    }
  }
  describe("The P06"){
    it("should return whether a list is palindrome :: true"){
      val input = List(1,2,2,1)
      val result = isPalindrome(input)

      result should be (true)
    }
    it("should return whether a list is palindrome :: false"){
      val input = List(1,2,2,2)
      val result = isPalindrome(input)

      result should be (false)
    }
  }


}

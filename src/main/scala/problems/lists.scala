package problems

import annotation.tailrec

object lists {

  //// P01: Find the last element of a list.
  //
  //  Find the last element of a list.
  //     Example:
  //     scala> last(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 8
  //
  //// Built-in solution
  val last_element_builtin = (input_list: List[Any]) => input_list.last

  //// Recursive approach
  @tailrec
  def last_element_func(input_list: List[Any], result: Any = null): Any = {
    input_list match {
      case Nil => result
      case (x::xs) => last_element_func(xs, x)
    }
  }

  //// P02: Find the last but one element of a list.
  //
  //     Example:
  //     scala> penultimate(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 5
  //
  //// Built-in solution
  val penultimate_builtin = (input_list: List[Any]) => input_list.init.last

  //// Recursive approach
  @tailrec
  def penultimate[A](input_list:List[A]): A = {
    input_list match {
      case head :: _ :: Nil => head
      case head :: tail => penultimate(tail)
      case _ => throw new NoSuchElementException
    }
  }

  //// P03: Find the Kth element of a list.
  //
  //     By convention, the first element in the list is element 0.
  //
  //     Example:
  //     scala> nth(2, List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 2
  //
  //// Built-in approach
  val nth_builtin = (element: Int, input_list: List[Any]) => {
    if(element >= 0 && element <= input_list.size) {input_list(element-1)}
    else {throw new NoSuchElementException}
  }

  //// Recursive approach
  @tailrec
  def nth_recursive[A](element: Int, input_list:List[A]): A = {
    (element-1,input_list) match {
      case (0, head :: _ )    => head
      case (_, head :: tail ) => nth_recursive(element-1, tail)
      case (_, Nil) => throw new NoSuchElementException
    }
  }

  //// Recursive approach with counter
  def nth_counter[A](element: Int, input_list:List[A]): A = {
    @tailrec def _nth_counter[A](element: Int, input_list:List[A], count: Int): A = {
      if (count > input_list.size) throw new NoSuchElementException
      else {
        count match {
          case 0 => input_list.head
          case _ => _nth_counter(element-1, input_list.tail, count-1)
        }
      }
    }
    _nth_counter(element, input_list, element-1)
  }

  //// P04: Find the number of elements of a list.
  //
  //     Example:
  //     scala> length(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 6
  //
  //// Built-in solution
  val length_builtin = (input_list: List[Any]) => {
    if(input_list == Nil) 0
    else input_list.size
  }

  //// Recursive approach
  def length_recursive[A](input_list: List[A]): Int = {
    @tailrec def _length_recursive[A](input_list: List[A], count: Int = 0): Int = {
      input_list match {
        case Nil => count
        case head :: tail => _length_recursive(tail, count+1)
      }
    }
    _length_recursive(input_list)
  }

  //// Pure functional solution
  val length_functional = (input_list: List[Any]) => {
    input_list.foldLeft(0){ (c, _) => c + 1 }
  }


  //// P05: Reverse a list.
  //     Example:
  //     scala> reverse(List(1, 1, 2, 3, 5, 8))
  //     res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  //
  //// Built-in method
  val reverse = (input_list:List[Any]) => input_list.reverse

  //// Functional approach
  def reverse_func[A](input_list:List[A]): List[A] = {
    @tailrec def _reverse_func[A](input_list:List[A], res: List[A] = Nil): List[A] = {
      input_list match {
        case Nil => res
        case head :: tail => _reverse_func(tail, head :: res)
      }
    }
    _reverse_func(input_list)
  }

  //// More pure functional approach
  def reverse_pure[A](input_list:List[A]): List[A] = {
    input_list.foldLeft(List[A]()){(a,h) => h :: a}
  }

  //// P06: Find out whether a list is a palindrome.
  //
  //     Example:
  //     scala> isPalindrome(List(1, 2, 3, 2, 1))
  //     res0: Boolean = true
  ////
  def isPalindrome(input_list:List[Int]): Boolean = {
    input_list == input_list.foldLeft(List[Int]()){ (a,h) => h :: a}
  }

  //// P07: Flatten a nested list structure.
  //
  //     Example:
  //     scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  //     res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  ////
  def listFlattened[A](input_list:List[A]): List[A] = {
    input_list flatMap {
      case input: List[A] => listFlattened(input)
      case o => List(o)
    }
  }

//// P08: Eliminate consecutive duplicates of list elements.
//
//     If a list contains repeated elements they should be replaced with a
//     single copy of the element.  The order of the elements should not be
//     changed.
//
//     Example:
//     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
////

// tail recursive
def compress[A](input_list: List[A]): List[A] = {
  @tailrec def _compress[A](input_list: List[A], res: List[A] = Nil): List[A] = {
    input_list match {
      case head :: tail => _compress(tail.dropWhile(_ == head), head :: res)
      case Nil => res.reverse
    }
  }
  _compress(input_list)
}

// fold approach
def compressFold[A](input_list: List[A]): List[A] = {
  input_list.foldRight(List[A]()){ (acc, i) =>
    if (i.isEmpty || i.head != acc) acc :: i
    else i
  }
}

//// P09: Pack consecutive duplicates of list elements into sublists.
//
//     If a list contains repeated elements they should be placed in separate
//     sublists.
//
//     Example:
//     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
////

//// P10: Run-length encoding of a list.
//
//     Use the result of problem P09 to implement the so-called run-length
//     encoding data compression method.  Consecutive duplicates of elements are
//     encoded as tuples (N, E) where N is the number of duplicates of the
//     element E.
//
//     Example:
//     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
////



}

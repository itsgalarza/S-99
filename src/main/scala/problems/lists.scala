package problems

object lists {

  //P01: Find the last element of a list.
  val last_element_builtin = (input_list: List[Any]) => input_list.last

  def last_element_func(input_list: List[Any], result: Any = null): Any = {
    input_list match {
      case Nil => result
      case (x::xs) => last_element_func(xs, x)
    }
  }

  //P02: Find the last but one element of a list.
  val penultime_builtin = (input_list: List[Any]) => input_list.init.last

  def penultime[A](input_list:List[A]): A = {
    input_list match {
      case head :: _ :: Nil => head
      case head :: tail => penultime(tail)
      case _ => throw new NoSuchElementException
    }
  }

  //TO-DO
  //P03: Find the Kth element of a list.

  //TO-DO
  //P04: Find the number of elements of a list.

  //TO-DO
  //P05: Reverse a list.

  //TO-DO
  //P06: Find out whether a list is a palindrome.

}

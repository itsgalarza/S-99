package problems

object easy {

  //P01
  val last_element_simplest = (input_list: List[Any]) => input_list.last

  def last_element_func(input_list: List[Any], result: Any = null): Any = {
    input_list match {
      case Nil => result
      case (x::xs) => last_element_func(xs, x)
    }
  }

}

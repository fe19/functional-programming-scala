package chapter3

object List {

  enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])


  def main(args: Array[String]): Unit = {
    println("Test")
  }

}

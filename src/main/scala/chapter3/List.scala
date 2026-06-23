package chapter3

/*
 * learnings
 * - concatenate              a ++ b
 * -
 * -
 * -
 * -
 * -
 * -
 * -
 * -
 */

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List {

  def main(args: Array[String]): Unit = {
    println("Test")
  }

}

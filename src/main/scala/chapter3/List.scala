package chapter3

/*
 * learnings
 * - concatenate              a ++ b
 * - covariance               List[+A]
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
    val l: List[String] = List.Cons("a", List.Cons("b", List.Nil))
    println(l)
  }

}

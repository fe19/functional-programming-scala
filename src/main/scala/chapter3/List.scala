package chapter3

/*
 * learnings
 * - concatenate              a ++ b
 * - covariance               List[+A]
 * - variadic function        accept zero or more arguments. E.g., def apply[A](as: A*)
 * - pattern matching         ints match case Nil => 0  case Cons(x, xs) => x + sum(xs)
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

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def main(args: Array[String]): Unit = {
    val l: List[String] = List.Cons("a", List.Cons("b", List.Nil))
    println(l)
  }

}

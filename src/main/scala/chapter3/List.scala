package chapter3

/*
 * learnings
 * - concatenate              a ++ b
 * - covariance               List[+A]
 * - variadic function        accept zero or more arguments. E.g., def apply[A](as: A*)
 * - Recursive List DS        List(1,2,3) = Cons(1, Cons(2, Cons(3, Nil)))
 * - pattern matching         similar like a fancy switch statement. E.g., ints match case Nil => 0  case Cons(x, xs) => x + sum(xs)
 *                            List(1,2,3) match {case _ => 42} = 42 since _ matches any expression
 *                            List(1,2,3) match {case Cons(h,_) => h} = 1
 *                            List(1,2,3) match {case Cons(_,h) => h} = List(2,3)
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

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1
    case Cons(x, xs) => x * product(xs)

  def main(args: Array[String]): Unit = {
    val l: List[String] = List.Cons("a", List.Cons("b", List.Nil))
    val l1 = List(1,2,3)
    val l2 = List(1.0,2.0,3.0)
    println(sum(l1))
    println(product(l2))
  }

}

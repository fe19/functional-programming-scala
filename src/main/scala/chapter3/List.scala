package chapter3

import scala.annotation.tailrec

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
 * - data sharing             do not copy lists, reuse it. E.g., add 1 to xs -> Cons(1, xs)
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

  // ex 3.2 (modifying lists)
  def tail[A](xs: List[A]): List[A] = xs match
    case Nil => Nil
    case Cons(x, xs) => xs

  // ex 3.3 (replace first element)
  def setHead[A](xs: List[A], x:A): List[A] = xs match
    case Nil => Nil
    case Cons(_, xs) => Cons(x, xs)

  // ex 3.4 (drop first n elements)
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    if n <= 0 then as
    else as match
      case Nil => Nil
      case Cons(_, as) => drop(as, n-1)

  // ex 3.5 (drop as long predicate matches)
  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Cons(h, tl) =>
      if f(h) then dropWhile(tl, f)
      else as
    case Nil => Nil

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match
    case Nil => a2
    case Cons(h, ts) =>
      Cons(h, append(ts, a2))

  def main(args: Array[String]): Unit = {
    val l: List[String] = List.Cons("a", List.Cons("b", List.Nil))
    val l1 = List(1,2,3)
    val l2 = List(1.0,2.0,3.0)
    println(sum(l1))
    println(product(l2))
    println(tail(l1))
    println(setHead(l1,3))
    println(drop(l1,2))
    println(dropWhile(l1, x => x < 2))
    println(append(l1, l2))
  }

}

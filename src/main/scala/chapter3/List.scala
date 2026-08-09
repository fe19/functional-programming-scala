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
 * - function arguments       avoid duplication by generalizing and put specialization into function argument
 * - anonymous functions      _ + _ = (x,y) => x + y
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

  // ex 3.2 (removing first element of a list). Complexity is O(1)
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

  // Add all elements of list a2 to the end of list a1. O(n) where n is the length of list a1
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match
    case Nil => a2
    case Cons(h, ts) =>
      Cons(h, append(ts, a2))

  // ex 3.6 (remove last element of a list). O(n) and not tail recursion
  def init[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(h, ts) => Cons(h, init(ts))

  // fold takes a list, a starting value, and a combining function. It combines all elements into a single value.
  def foldRight[A,B](as: List[A], acc: B, f: (A,B) => B): B = as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumFold(as: List[Int]): Int =
    foldRight(as, 0, (x,y) => x + y)

  def productFold(as: List[Double]): Double =
    foldRight(as, 1.0, _ * _)

  // ex 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)

  // ex 3.10 (fold with tail recursion)
  @tailrec
  def foldLeft[A,B](as: List[A], acc: B, f: (B,A) => B): B = as match
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc,x), f)

  // ex 3.11 (use fold left)
  def sumFoldLeft(as: List[Int]): Int =
    foldLeft(as, 0, (x,y) => x + y)

  // ex 3.12 (reverse a list)
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (acc, a) => Cons(a, acc))

  // ex 3.13 (fold right)
  def foldRightViaLeft[A,B](as: List[A], acc:B, f:(A,B) => B): B =
    foldLeft(reverse(as),acc, (b,a) => f(a,b))

  // ex.3.14 (append with fold)
  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_,_))

  // concatenate lists
  def concatLists[A](as: List[A]*): List[A] =
    if as.isEmpty then Nil
    else  append(as.head, concatLists(as.tail*))

  // ex 3.15 (concatenate list of lists)
  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A], append)

  // ex 3.16 transform list by adding 1
  def addOne(as: List[Int]): List[Int] = {
    foldRight(as, Nil: List[Int], (i, acc) => Cons(i + 1, acc))

    // ex 3.17 transform double to string



  def main(args: Array[String]): Unit = {
    val l: List[String] = List.Cons("a", List.Cons("b", List.Nil))
    val l1 = List(1,2,3)
    val l2 = List(1.0,2.0,3.0)
    println(sum(l1))
    println(sumFold(l1))
    println(product(l2))
    println(productFold(l2))
    println(tail(l1))
    println(setHead(l1,3))
    println(drop(l1,2))
    println(dropWhile(l1, x => x < 2))
    println(append(l1, l2))
    println(init(l1))
    print("ex 3.8: ")
    println(foldRight(List(1,2,3), Nil: List[Int], Cons(_,_)))
    println(length(l1))
    println(sumFoldLeft(l1))
    println(reverse(l1))
    println(appendViaFold(l1, l2))
    println(concat(List(l1,l2)))
    println(addOne(l1))
  }

}

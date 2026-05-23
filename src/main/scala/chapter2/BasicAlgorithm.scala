package chapter2

import scala.annotation.tailrec

/*
 * learnings
 * - recursive loops:         def loop(n: Int): Int   - @tailrec for tail-recursive optimization, i.e., stable stack
 * - recursive methods:       def go(n: Int, c: Int, ...): Int
 * - higher-order functions:  def format(n: Int, f: Int => Int)
 * - anonymous functions      (x: Int) => x == 9
 * - anonymous arguments      _ > _ simpler as (x: Int, y: Int) => x > y
 * - generics:                def find[A]
 * - placeholder              (b: B) => ???   placeholder common to use for incrementally build up functionality
 * - currying:               signature often defines implementation
 * -
 */

object BasicAlgorithm {

  // recursive method
  def factorial(n: Int): Int =
    @tailrec
    def go(n: Int, acc: Int): Int = {
      // println(s"go($n, $acc)")
      if n <= 0 then acc
      else go(n-1, n * acc)
    }
    go(n, 1)


  // tail recursive loop
  def tailRecLoop(n: Int): Unit =
    @tailrec
    def loop(i: Int): Unit =
      println(s"i=$i")
      if i < n - 1 then loop(i + 1)

    loop(0)

  // exercise 2.1
  def fib(n: Int): Int =
    @tailrec
    def go(n: Int, curr: Int, next: Int): Int =
      // println(s"go($n,$curr,$next)")
      if n<=0 then curr
      else go(n-1, next, curr + next)
    go(n,0,1)

  // generic / polymorphic function / method
  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n+1)
    loop(0)

  // exercise 2.2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @tailrec
    def loop(n: Int): Boolean =
      if n == as.length then true
      else if gt(as(n-1), as(n)) then false
      else loop(n + 1)
    loop(1)

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a,b)

  // exercise 2.3 (currying)
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  // exercise 2.4 (uncurry)
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a,b) => f(a)(b)

  // exercise 2.5 (composition)
  def compose[A,B,C](f:  B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(factorial(5))
    println(fib(6))
    println(findFirst(Array(1,2,3), (x: Int) => x==2))
    println(isSorted(Array(1,2,3), (x: Int, y: Int) => x > y))
    println(isSorted(Array(1,2,1), _ > _))
    println(isSorted(Array(3,2,1), _ < _))
    println(isSorted(Array(1,2,3), _ < _))


    def add(x: Int, y: Int) = x + y
    val addFive: Int => Int = partial1(5, add)
    println(addFive(10))
    println(add(4,5))
    //println(partial1(5, (x: Int, y: Int => x + y)))

    def multiply(x: Int, y: Int) = x * y
    val composed: Int => Int = compose(addFive, addFive)
    println(composed)
  }
}

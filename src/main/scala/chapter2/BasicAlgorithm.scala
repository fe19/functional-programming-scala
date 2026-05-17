package chapter2

import scala.annotation.tailrec

/*
 * learnings
 * - recursive loops:         def loop(n: Int): Int
 * - recursive methods:       def go(n: Int, c: Int, ...): Int
 * - higher-order functions:  def format(n: Int, f: Int => Int)
 * - generics:                def find[A]
 * - signature:               often defines implementation
 *
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

  def main(args: Array[String]): Unit = {
    println(factorial(5))
    println(fib(6))
    println(findFirst(Array(1,2,3), (x: Int) => x==2))
  }
}

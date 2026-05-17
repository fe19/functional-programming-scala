package chapter2

import scala.annotation.tailrec

object BasicAlgorithm {

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

  def main(args: Array[String]): Unit = {
    println(factorial(5))
    println(fib(6))
  }
}

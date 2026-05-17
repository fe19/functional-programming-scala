package chapter2

object Format {

  // A higher-order function that takes a function f as an argument
  private def formatResult(name: String, n: Int, f: Int => Int) =
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))

  @main def printAbsAndFactorial(): Unit =
    println(formatResult("absolute value", -42, math.abs))
    println(formatResult("factorial", 7, BasicAlgorithm.factorial))
}

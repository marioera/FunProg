package chapter2

import scala.annotation.tailrec

object Samples {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int): Unit = {
    val msg = "The absolute values of %d is %d"
    msg.format(x, abs(x))
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop_fib(p1: Int, p2: Int, n: Int): Int = {
      if (n == 0) 0
      else if (n == 1) p2
      else loop_fib(p2, p1 + p2, n - 1)
    }

    loop_fib(0, 1, n)

  }

  (0 to 10).map(fib).map(i => print(i + " "))
  //println(fib(3))

  def main(args: Array[String]): Unit = {
    println(formatAbs(-5))
  }
}
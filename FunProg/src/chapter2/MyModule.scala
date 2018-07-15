package chapter2

import scala.annotation.tailrec

object MyModule {
  //2.2
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int): String = {
    val msg = "The absolute values of %d is %d"
    msg.format(x, abs(x))
  }

  //2.5
  def factorial(n: Int): Int = {
    @tailrec
    def loop_factorial(acc: Int, n: Int): Int = {
      if (n == 0) acc
      else loop_factorial(n * acc, n - 1)
    }

    loop_factorial(1, n)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  // Exercise 1
  def fib(n: Int): Int = {
    @tailrec
    def loop_fib(p1: Int, p2: Int, n: Int): Int = {
      if (n == 0) 0
      else if (n == 1) p2
      else loop_fib(p2, p1 + p2, n - 1)
    }

    loop_fib(0, 1, n)
  }

  // Exercise 2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(count: Int): Boolean = {
      if (as.length <= 0 || count == as.length) true
      else if (count < as.length - 1 && !gt(as(count + 1), as(count))) false
      else loop(count + 1)
    }

    loop(0)
  }

  // Exercise 3
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    f(a, _)
  }

  // Exercise 4
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _)
  }

  // Exercise 5
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Exercise 6
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-5))
    println(factorial(10))
    println(formatResult("absolute value", -10, abs))
    println(formatResult("factorial", 10, factorial))
    println(formatResult("increment", 10, (x) => x + 1))
    println(formatResult("increment1", 10, (x: Int) => x + 1))
    println(formatResult("increment2", 10, x => x + 1))
    println(formatResult("increment3", 10, _ + 1))
    println(formatResult("increment4", 10, x => { val r = x + 1; r }))
    println(formatResult("increment5", 10, x => {
      val r = x + 1
      r
    }))

    // Exercise 1
    (0 to 10).map(fib).map(i => print(i + " "))

    // Exercise 2 -
    val fInt = (i: Int, j: Int) => if (i > j) true else false
    val fDouble = (i: Double, j: Double) => if (i > j) true else false
    println(isSorted(Array(), fInt))
    println(isSorted(Array(1, 2, 3, 4), fInt))
    println(isSorted(Array(4, 3, 2, 1), fInt))
    println(isSorted(Array(1, 3, 2, 4), fInt))
    println(isSorted(Array(1.0, 3.2, 2.3, 4.5), fDouble))
    println(isSorted(Array(1.1, 2.2, 3.3, 4.4), fDouble))

    // Exercise 3 - Partial Function
    val f = (a: Int, b: Boolean) => if (a > 0 && b) "%d is >".format(a) else "%d is < or display is %b".format(a, b)
    val p1 = partial1(3, f)
    println(p1(true))
    println(p1(false))

    // Exercise 4 - Currying
    val curry1 = curry(f)
    println(curry1(1)(true))
    println(curry1(1)(false))
    println(curry1(-1)(true))
    println(curry1(-1)(false))

    // Exercise 5 - Uncurry
    val uncurry1 = uncurry(curry1)
    println(uncurry1(1, true))
    println(uncurry1(1, false))
    println(uncurry1(-1, true))
    println(uncurry1(-1, false))

    // Exercise 6 - Compose
    val fBtoC = (b: Boolean) => if (b) "Result is true" else "Result is false"
    val gAtoB = (a: Int) => a > 0
    val composed = compose(fBtoC, gAtoB)
    println(composed(1))
    println(composed(-1))
  }
}
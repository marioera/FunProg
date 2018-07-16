package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum[A](as: List[A], base: A)(f: (A, A) => A): A = as match {
    case Nil => base
    case Cons(h, t) => f(h, sum(t, base)(f))
  }

  def product[A](as: List[A], base: A)(f: (A, A) => A): A = as match {
    case Nil => base
    case Cons(h, t) => f(h, product(t, base)(f))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(l: List[Int]) =
    foldRight(l, 0)(_ + _)

  def product2(l: List[Int]) =
    foldRight(l, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("tail of empty list")
    case Cons(h, t) => t
  }

  // Exercise 3
  def drop[A](as: List[A], n: Int): List[A] = {
    if (n == 0) as
    else as match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    }
  }

  // Exercise 4
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t)(f) else Cons(h, t)
  }

  // Exercise 5
  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => sys.error("setHead on empty list")
    case Cons(h, t) => Cons(head, Cons(h, t))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // Exercise 6
  def init[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("init of empty list")
    case Cons(h, Cons(ht, Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
  }

  // Exercise 9
  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
  }

  def lengthFR[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  // Exercise 10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 11
  def sumFL(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def productFL(l: List[Int]): Int = {
    foldLeft(l, 1)(_ * _)
  }

  def lengthFL[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  // Exercise 12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))
  }

  // Exercise 13
  def foldLeftFR[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  def foldRightFL[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def foldLeftFR_1[A, B](as: List[A], z: B)(combiner: (B, A) => B): B = {
    type BtoB = B => B
    def innerIdent: BtoB = (b: B) => b
    def combinerDelayer: (A, BtoB) => BtoB =
      (a: A, delayFunc: BtoB) => (b: B) => delayFunc(combiner(b, a))
    def go: BtoB = foldRight(as, innerIdent)(combinerDelayer)
    go(z)
  }

  /*
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
  	}
   */

  /*
  foldLeftFR_1(List(1, 2, 3), Nil: List[Int])((b: B, a: A) => Cons(a, b))
  foldRight(List(1, 2, 3), innerIdent)(combineDelayer)(Nil)
  combineDelayer(1, foldRight(List(2, 3), innerIdent)(combineDelayer)(Nil))

  def delay3(b: B) = innerIdent(combiner(b, 3))
  def delay2(b: B) = delay3(combiner(b, 2))
  def delay1(b: B) = delay2(combiner(b, 1))
  delay1(Nil)

  innerIdent(combiner(combiner(combiner(Nil, 1), 2), 3))
  */

  // Exercise 14
  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  // Exercise 15
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  // Exercise 16
  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  // Exercise 17
  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, s) => Cons(h.toString, s))
  }

  // Exercise 18
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))
  }

  // Exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
  }

  // Exercise 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight(l, Nil: List[B])((a, b) => append(f(a), b))
  }

  // Exercise 21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  // Exercise 22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, addPairwise(at, bt))
  }

  // Exercise 23
  def zipWith[A, C](a: List[A], b: List[A])(f: (A, A) => C): List[C] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
  }

  // Exercise 24
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(ph, pt)) if h == ph => startsWith(t, pt)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case Cons(h, t) => if (startsWith(l, sub)) true else hasSubsequence(t, sub)
  }

  def main(args: Array[String]): Unit = {
    val example = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    println(example)
    val example2 = List(6, 7, 8, 9, 10)
    println(example2)
    val nilList = List()
    val sum0 = sum(example2, 0)(_ + _)
    println("sum: " + sum0)
    val product0 = product(example2, 1)(_ * _)
    println("product: " + product0)

    // Exercise 2
    println("tail: " + tail(example))
    // Exercise 3
    println("drop: " + drop(example, 3))
    // Exercise 4
    println("dropWhile: " + dropWhile(example)(_ <= 4))
    // Exercise 5
    println("setHead: " + setHead(example, 0))
    // Exercise 6
    println("init: " + init(example))

    val sum1 = sum2(example2)
    println("sum2: " + sum1)
    val product1 = product2(example2)
    println("product2: " + product1)

    // Exercise 9
    println("length: " + length(example))
    println("lengthFR: " + lengthFR(example))

    // Exercise 10
    println("foldLeft: " + foldLeft(example, 0)(_ + _))
    println("foldLeft: " + foldLeft(example, 1)(_ * _))

    // Exercise 11
    println("sumFL: " + sumFL(example))
    println("productFL: " + productFL(example))
    println("lengthFL: " + lengthFL(example))

    // Exercise 12
    println("reverse: " + reverse(example))

    // Exercise 13
    println("List: " + example)
    println("List foldRight: " + foldRight(example, Nil: List[Int])(Cons(_, _)))
    println("List foldRightFL: " + foldRightFL(example, Nil: List[Int])(Cons(_, _)))
    println("List foldLeft: " + foldLeft(example, Nil: List[Int])((acc, h) => Cons(h, acc)))
    println("List foldLeftFR: " + foldLeftFR(example, Nil: List[Int])((acc, h) => Cons(h, acc)))
    println("List foldLeftFR_1: " + foldLeftFR_1(example, Nil: List[Int])((acc, h) => Cons(h, acc)))

    // Exercise 14
    println("append foldRight: " + appendViaFold(example, example2))

    // Exercise 15
    val listOfLists = List(example, example2)
    println("concat: " + concat(listOfLists))

    // Exercise 16
    println("add1: " + add1(example))

    // Exercise 17
    val listDouble = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Cons(5.0, Nil)))))
    println("doubleToString: " + doubleToString(listDouble))

    // Exercise 18
    println("map add1: " + map(example)(_ + 1))
    println("map doubleToString: " + map(listDouble)(_.toString))

    // Exercise 19
    println("filter: " + filter(example)(_ % 2 == 0))
    println("filter: " + filter(example)(_ % 2 != 0))

    // Exercise 20
    println("flatMap: " + flatMap(example)(i => List(i, i)))

    // Exercise 21
    println("filterViaFlatMap: " + filterViaFlatMap(example)(_ % 2 == 0))
    println("filterViaFlatMap: " + filterViaFlatMap(example)(_ % 2 != 0))

    // Exercise 22
    println("addPairwise: " + addPairwise(example, example2))

    // Exercise 23
    println("zipWith: " + zipWith(example, example2)(_ + _))
    println("zipWith: " + zipWith(example, example2)(_ * _))

    // Exercise 24
    println("hasSubsequence: " + hasSubsequence(example, example2))
    println("hasSubsequence: " + hasSubsequence(example, List(4, 5)))
  }
}
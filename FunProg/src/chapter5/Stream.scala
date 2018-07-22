package chapter5

import scala.annotation.tailrec

import Stream._
sealed trait Stream[+A] {
  // Exercise 1
  def toList: List[A] = {
    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, List()).reverse
  }

  // Exercise 2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1) => cons(h(), t().take(n - 1))
    case Cons(h, t) if (n == 1) => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  // Exercise 3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 4
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  // Exercise 5
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  // Exercise 6
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val stream0 = Empty
    val stream1 = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println("Stream Empty: " + stream0)
    println("Stream NonEmpty: " + stream1)

    // Exercise 1
    println("toList: " + stream0.toList)
    println("toList: " + stream1.toList)
    // Exercise 2
    println("take: " + stream0.take(5))
    println("take: " + stream1.take(5).toList)
    println("drop: " + stream0.drop(5))
    println("drop: " + stream1.drop(5).toList)
    // Exercise 3
    println("takeWhile: " + stream0.takeWhile((a: Int) => a <= 5))
    println("takeWhile: " + stream1.takeWhile(_ <= 5).toList)
    println("exists: " + stream1.exists(_ <= 5))
    println("exists: " + stream1.exists(_ > 10))
    // Exercise 3
    println("forAll: " + stream1.forAll(_ <= 5))
    println("forAll: " + stream1.forAll(_ <= 10))
    // Exercise 5
    println("takeWhile2: " + stream0.takeWhile2((a: Int) => a <= 5))
    println("takeWhile2: " + stream1.takeWhile2(_ <= 5).toList)
    // Exercise 6
    println("headOption: " + stream0.headOption)
    println("headOption: " + stream1.headOption)
    println("map: " + stream1.map(a => a * a).toList)
    println("filter: " + stream1.filter(a => a % 2 == 0).toList)
    println("append: " + stream1.append(stream1.filter(a => a % 2 == 0)).toList)
    println("flatMap: " + stream1.flatMap(a => Stream(a)).toList)
  }
}
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

  // Exercise 12
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (Empty, 0)))
      case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
      case _ => None
    }
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, b)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
    }
  }

  def zip[B](b: Stream[B]): Stream[(A, B)] = {
    zipWith(b)((_, _))
  }

  def zipWithAll[B, C](b: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
    unfold((this, b)) {
      case (Empty, Empty) => None
      case (Cons(ah, at), Empty) => Some((f(Some(ah()), None), (at(), Empty)))
      case (Empty, Cons(bh, bt)) => Some((f(None, Some(bh())), (Empty, bt())))
      case (Cons(ah, at), Cons(bh, bt)) => Some((f(Some(ah()), Some(bh())), (at(), bt())))
    }
  }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] = {
    zipWithAll(b)((_, _))
  }

  // Exercise 13
  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile(!_._2.isEmpty).forAll {
      case (a, b) => a == b
    }
  }

  // Exercise 14
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some((Cons(h, t), t()))
      case Empty => None
    }
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    this.tails.exists(_.startsWith(s))
  }

  // Exercise 15
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val lazyAcc = acc
      val bH = f(a, lazyAcc._1)
      (bH, cons(bH, lazyAcc._2))
    })._2
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

  val ones: Stream[Int] = cons(1, ones)

  // Exercise 7
  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  // Exercise 8
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  // Exercise 9
  def fibs: Stream[Int] = {
    def loop_fibs(n: Int, m: Int): Stream[Int] = {
      cons(n, loop_fibs(m, n + m))
    }

    loop_fibs(0, 1)
  }

  // Exercise 10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  // Exercise 11
  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) { case (n, m) => Some((n, (m, n + m))) }
    //unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s + 1))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(s => Some(s, s))
  }

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(_ => Some(1, 1))
  }

  def main(args: Array[String]): Unit = {
    val stream0 = Empty
    val stream1 = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val stream2 = Stream(1, 2, 3)
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

    println("ones take: " + ones.take(5).toList)
    println("ones exists: " + ones.exists(_ % 2 != 0))
    println("ones map: " + ones.map(_ + 1).take(5).toList)
    println("ones map: " + ones.map(_ + 1).exists(_ % 2 == 0))
    println("ones takeWhile: " + ones.takeWhile(_ == 1).take(5).toList)
    println("ones forAll: " + ones.forAll(_ != 1))

    // Exercise 7
    println("constant take: " + constant(10).take(5).toList)
    println("constant take: " + constant("a").take(5).toList)

    // Exercise 8
    println("from: " + from(10).take(5).toList)

    // Exercise 9
    println("fibs: " + fibs.take(40).toList)

    // Exercise 11
    println("fibsViaUnfold: " + fibsViaUnfold.take(40).toList)
    println("fromViaUnfold: " + fromViaUnfold(10).take(5).toList)
    println("constantViaUnfold: " + constantViaUnfold(10).take(5).toList)
    println("constantViaUnfold: " + constantViaUnfold("a").take(5).toList)
    println("onesViaUnfold: " + onesViaUnfold.take(5).toList)

    // Exercise 12
    println("mapViaUnfold: " + stream1.mapViaUnfold(a => a * a).toList)
    println("takeViaUnfold: " + stream0.takeViaUnfold(5))
    println("takeViaUnfold: " + stream1.takeViaUnfold(5).toList)
    println("takeWhileViaUnfold: " + stream0.takeWhileViaUnfold((a: Int) => a <= 5))
    println("takeWhileViaUnfold: " + stream1.takeWhileViaUnfold(_ <= 5).toList)
    println("zipWith: " + stream1.zipWith(ones)((a, b) => (a + b, b)).take(20).toList)
    println("zip: " + stream1.zip(ones).take(20).toList)
    println("zipAll: " + stream1.zipAll(ones).take(20).toList)

    // Exercise 13
    println("startsWith: " + stream1.startsWith(ones))
    println("startsWith: " + stream1.startsWith(stream2))

    // Exercise 14
    println("tails: " + stream1.tails.map(_.toList).toList)
    println("hasSubsequence: " + stream1.hasSubsequence(ones))
    println("hasSubsequence: " + stream1.hasSubsequence(stream2))

    // Exercise 15
    println("scanRight: " + stream1.scanRight(0)(_ + _).toList)
  }
}
package chapter4

sealed trait Either[+E, +A] {
  // Exercise 7
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(v) => Left(v)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(v) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(a) => b match {
      case Right(b) => Right(f(a, b))
      case Left(b) => Left(b)
    }
    case Left(a) => Left(a)
  }

  def map3[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(aa => b.map(bb => f(aa, bb)))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: Seq[Double]): Either[String, Double] = xs match {
    case Nil => Left("mean of empty list!")
    case _ => Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }
  }

  // Exercise 8
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    a.foldRight(Right(Nil): Either[E, List[A]])((x, y) => x.map2(y)(_ :: _))
  }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    a.foldRight(Right(Nil): Either[E, List[B]])((a, b) => f(a).map2(b)(_ :: _))
  }

  def sequence2[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(a)(b => b)
  }

  def main(args: Array[String]): Unit = {
    val list0: List[Double] = List()
    val list1: List[Double] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println("mean: " + mean(list0))
    println("mean: " + mean(list1))
    println("safeDiv: " + safeDiv(5, 3))
    println("safeDiv: " + safeDiv(5, 0))

    // Exercise 7
    val left0 = Left(0)
    val left1 = Left(1)
    val right0 = Right(5)
    val right1 = Right(6)
    println("map: " + right0.map(_ + 10))
    println("map3: " + left0.map3(left1)((a: Int, b: Int) => (a * 10) + (b * 100)))
    println("map3: " + left0.map3(right1)((a: Int, b: Int) => (a * 10) + (b * 100)))
    println("map3: " + right0.map3(left1)((a: Int, b: Int) => (a * 10) + (b * 100)))
    println("map3: " + right0.map3(right1)((a: Int, b: Int) => (a * 10) + (b * 100)))

    // Exercise 8
    val eitherList0 = Nil
    val eitherList1 = List(Right(1), Right(2), Left("Error :D"), Right(4))
    val eitherList2 = List(Right(1), Right(2), Right(3), Right(4))

    println("sequence: " + sequence(eitherList0))
    println("sequence: " + sequence(eitherList1))
    println("sequence: " + sequence(eitherList2))

    println("sequence2: " + sequence2(eitherList0))
    println("sequence2: " + sequence2(eitherList1))
    println("sequence2: " + sequence2(eitherList2))

    val eitherList3 = List(1, 3, 4, 5, 7)
    val eitherList4 = List(1, 3, 5, 7)

    println("traverse: " + traverse(eitherList0)((a: Int) => if (a % 2 == 1) Right(a) else Left("Error :D!")))
    println("traverse: " + traverse(eitherList3)((a: Int) => if (a % 2 == 1) Right(a) else Left("Error :D!")))
    println("traverse: " + traverse(eitherList4)((a: Int) => if (a % 2 == 1) Right(a) else Left("Error :D!")))

  }
}
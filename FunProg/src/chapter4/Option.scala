package chapter4

sealed trait Option[+A] {
  // Exercise 1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def filter_1(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  // Exercise 2
  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _ => Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    //def v(m: Option[Double]) = mean(xs.map(x => math.pow(x - m.getOrElse(0.0d), 2)))
    //v(mean(xs))
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // Exercise 3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def map3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  // Exercise 4
  def bothMatch2(pat: String, pat2: String, s: String): Option[Boolean] = {
    map2(Some(pat), Some(pat2))((a, b) => a.isEmpty() && b.isEmpty())
  }

  def main(args: Array[String]): Unit = {
    val option0 = None
    val option1 = Some(5)
    val option2 = Some(10.0d)
    val list0: List[Double] = List()
    val list1: List[Double] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    // Exercise 1
    println("None: " + option0)
    println("Some: " + option1)
    println("map: " + option1.map(_ * 5))
    println("flatMap: " + option1.flatMap(a => if (a > 0) Some(a) else None))
    println("flatMap: " + option1.flatMap(a => if (a > 0) None else Some(a)))
    println("flatMap_1: " + option1.flatMap_1(a => if (a > 0) Some(a) else None))
    println("flatMap_1: " + option1.flatMap_1(a => if (a > 0) None else Some(a)))
    println("getOrElse: " + option0.getOrElse(50))
    println("getOrElse: " + option1.getOrElse(50))
    println("orElse: " + option0.orElse(Some(100)))
    println("orElse: " + option1.orElse(Some(100)))
    println("orElse_1: " + option0.orElse_1(Some(100)))
    println("orElse_1: " + option1.orElse_1(Some(100)))
    println("filter: " + option1.filter(_ > 4))
    println("filter: " + option1.filter(_ < 4))
    println("filter_1: " + option1.filter_1(_ > 4))
    println("filter_1: " + option1.filter_1(_ < 4))

    // Exercise 2
    println("mean: " + mean(list0))
    println("mean: " + mean(list1))
    println("variance: " + variance(list0))
    println("variance: " + variance(list1))

    //Exercise 3
    println("map2: " + map2(option1, option2)(_ + "|" + _))
    println("map3: " + map2(option1, option2)(_ + "|" + _))

    //Exercise 4
    println("bothMatch2: " + bothMatch2("", "", ""))
    println("bothMatch2: " + bothMatch2("Hello", "World", ""))
  }
}



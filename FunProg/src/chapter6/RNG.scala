package chapter6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    ((i1, i2), rng2)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    rng.nextInt match { case (i, rng2) => (i % 2 == 0, rng2) }
  }

  // Exercise 1
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = positiveInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt

    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  // Exercise 4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop_ints(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count <= 0) (acc, rng)
      else {
        val (i, r) = rng.nextInt
        loop_ints(count - 1, i :: acc)(r)
      }
    }

    loop_ints(count, List())(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] =
    rng => rng.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 5
  def positiveMax(n: Int): Rand[Int] = {
    map(positiveInt)(_ % n)
  }

  // Exercise 6
  val _double: Rand[Double] =
    map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 7
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
      }
  }

  def _intDouble: Rand[(Int, Double)] = {
    map2(int, double)((_, _))
  }

  def _doubleInt: Rand[(Double, Int)] = {
    map2(double, int)((_, _))
  }

  // Exercise 8
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))
  }

  def _ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  // Exercise 9
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }
  }

  def positiveIntViaFlatMap: Rand[Int] = {
    flatMap(int)(i => if (i != Int.MinValue) unit(i.abs) else positiveIntViaFlatMap)
  }

  // Exercise 10
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))
  }

  def _doubleIntViaFlatMap: Rand[(Double, Int)] = {
    map2ViaFlatMap(double, int)((_, _))
  }

  def mapGeneral[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) = {
    rng =>
      {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }
  }

  def main(args: Array[String]): Unit = {
    val rnd0 = simple(10L)
    val rnd1 = simple(50L)

    println("simple: " + rnd0.nextInt._1)
    println("simple: " + rnd0.nextInt._2.nextInt._1)
    println("randomPair: " + randomPair(simple(50L))._1)

    // Exercise 1
    println("positiveInt: " + positiveInt(positiveInt(rnd0)._2)._1)
    // Exercise 2
    println("double: " + double(rnd0)._1)
    println("double: " + double(double(rnd0)._2)._1)
    // Exercise 3
    println("intDouble: " + intDouble(rnd0)._1)
    println("doubleInt: " + doubleInt(rnd0)._1)
    println("double3: " + double3(rnd0)._1)
    // Exercise 4
    println("ints: " + ints(10)(rnd0))

    println("int: " + int)
    println("int: " + int(rnd0))
    println("unit: " + unit(10L)(rnd0))
    println("map: " + map(int)(_ / 50000)(rnd0))

    // Exercise 5
    println("positiveMax: " + positiveMax(5)(rnd0))
    println("positiveMax: " + positiveMax(5)(rnd1))

    // Exercise 6
    println("_double: " + _double(rnd0)._1)
    println("_double: " + _double(_double(rnd0)._2)._1)

    // Exercise 7
    println("_intDouble: " + _intDouble(rnd0))
    println("_doubleInt: " + _doubleInt(rnd0))

    // Exercise 8
    println("List.fill: " + List.fill(10)(int))
    println("_ints: " + _ints(10)(rnd0))

    // Exercise 9
    println("positiveIntViaFlatMap: " + positiveIntViaFlatMap(rnd0))

    // Exercise 10
    println("_doubleIntViaFlatMap: " + _doubleIntViaFlatMap(rnd0)._1)
    println("_doubleIntViaFlatMap: " + _doubleIntViaFlatMap(_doubleIntViaFlatMap(rnd0)._2)._1)

  }
}
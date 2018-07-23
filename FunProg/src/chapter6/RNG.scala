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

  def main(args: Array[String]): Unit = {
    val rnd0 = simple(50L)
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
  }
}
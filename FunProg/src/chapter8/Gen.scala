package chapter8

import chapter6.State
import chapter6.RNG

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop = {
    new Prop {
      def check = Prop.this.check && p.check
    }
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(sample.map2(g.sample)(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Int): Gen[List[A]] = {
    Gen.listOfN(size, this)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => this.listOfN(n))
  }
}

object Gen {

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.positiveInt).map(n => start + n % (stopExclusive - start)))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def listIfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    List.fill(n)(g).foldRight(unit[List[A]](List()))((s, acc) => s.map2(acc)(_ :: _))
  }

  def uniform: Gen[Double] = {
    Gen(State(RNG.double))
  }

  def choose(start: Double, stopExclusive: Double): Gen[Double] = {
    Gen(State(RNG.double).map(d => start + d * (stopExclusive - start)))
  }

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 != 0) n + 1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 == 0) n + 1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for {
    i <- choose(from, to)
    j <- if (i % 2 == 0) even(from, to) else odd(from, to)
  } yield (i, j)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def main(args: Array[String]): Unit = {
    val rng = RNG.simple(50L)
    println("boolean: " + Gen.boolean)
    println("boolean: " + Gen.boolean.sample)
    println("boolean: " + Gen.boolean.sample.run(rng))
    println("choose: " + Gen.choose(1, 5).sample.run(rng))
    println("listOfN - booleans: " + Gen.listOfN(10, Gen.boolean).sample.run(rng))
    println("listOfN - ints: " + Gen.listOfN(10, Gen.choose(1, 100)).sample.run(rng))
    println("listOfN - List[Ints]: " + Gen.listOfN(10, Gen.listOfN(5, Gen.choose(30, 40))).sample.run(rng))
    println("sameParity: " + Gen.listOfN(10, Gen.sameParity(1, 9)).sample.run(rng))
    println("listIfN_1 - booleans: " + Gen.listIfN_1(10, Gen.boolean).sample.run(rng))
    println("listIfN_1 - ints: " + Gen.listIfN_1(10, Gen.choose(1, 100)).sample.run(rng))
    println("listIfN_1 - List[Ints]: " + Gen.listIfN_1(10, Gen.listIfN_1(5, Gen.choose(30, 40))).sample.run(rng))
    println("sameParity: " + Gen.listIfN_1(10, Gen.sameParity(1, 9)).sample.run(rng))

    /*
    val intList = Gen.listOf(Gen.choose(0, 100))
    val prop =
      forAll(intList)(l => l.reverse.reverse == l) &&
        forAll(intList)(l => l.headOption == l.reverse.lastOption)
    prop.check
    */
  }
}
package chapter6

import State._
//type State[S, +A] = S => (A, S)

case class State[S, +A](run: S => (A, S)) {
  // Exercise 11
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](List()))((s, acc) => s.map2(acc)(_ :: _))
  }

  // Exercise 12
  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = {
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
      })))
      s <- get
    } yield (s.coins, s.candies)
  }

  def main(args: Array[String]): Unit = {
    val inputs = List(Coin, Turn, Coin, Turn)
    val machine = new Machine(true, 100, 0)

    println("Simulate: " + simulateMachine(inputs).run(machine))
  }
}
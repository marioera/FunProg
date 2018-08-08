package chapter7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.Executors

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
    def get(timeout: Long, units: TimeUnit) = get
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(timeout, units))

    private def compute(timeoutsMs: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis
        val ar = a.get(timeoutsMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis()
        val at = stop - start
        val br = b.get(timeoutsMs - at, TimeUnit.MILLISECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  // Exercise 3
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = {
    (es: ExecutorService) => UnitFuture(a)
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) =>
      {
        val af = a(es)
        val bf = b(es)
        Map2Future(af, bf, f)
      }
  }

  def fork[A](a: => Par[A]): Par[A] = {
    (es: ExecutorService) =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // Exercise 4
  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = {
    map2(l, unit(()))((a, _) => a.sorted)
  }

  def map[A, B](fa: Par[A])(f: A => B): Par[B] = {
    map2(fa, unit(()))((a, _) => f(a))
  }

  def sortParViaMap(l: Par[List[Int]]): Par[List[Int]] = {
    map(l)(_.sorted)
  }

  // Exercise 5
  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = {
    (es: ExecutorService) => UnitFuture((fa(es).get, fb(es).get))
  }

  def mapAsPrimitive[A, B](fa: Par[A])(f: A => B): Par[B] = {
    (es: ExecutorService) => UnitFuture(f(fa(es).get))
  }

  def map2ViaProduct[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    mapAsPrimitive(product(a, b)) { case (fa, fb) => f(fa, fb) }
  }

  // Exercise 6
  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  // Exercise 7
  def sequence[A](l: List[Par[A]]): Par[List[A]] = {
    l.foldRight[Par[List[A]]](unit(List()))((a, acc) => map2(a, acc)(_ :: _))
  }

  // Exercise 8
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = l.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  // Doesn't work. It's evaluating function while constructing response. Must delay evaluation.
  def parFilter2[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    def loop(l: List[A], acc: List[Par[A]]): List[Par[A]] = l match {
      case Nil => acc
      case h :: t => if (f(h)) loop(t, unit(h) :: acc) else loop(t, acc)
    }

    sequence(loop(l, List()))
  }

  def map3[A, B, C, D](fa: Par[A], fb: Par[B], fc: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(map2(fa, fb)((a, b) => (c: C) => f(a, b, c)), fc)(_(_))
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = {
    p(e).get == p2(e).get
  }

  def delay[A](a: => Par[A]): Par[A] = {
    (es: ExecutorService) => a(es)
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    es => if (a(es).get) ifTrue(es) else ifFalse(es)
  }

  // Exercise 15
  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es =>
      {
        val idx = run(es)(a).get
        run(es)(choices(idx))
      }
  }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))
  }

  // Exercise 16
  def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] = {
    es =>
      {
        val key = run(es)(a).get
        run(es)(choices(key))
      }
  }

  // Exercise 17
  def chooser[A, B](a: Par[A])(choices: A => Par[B]): Par[B] = {
    es =>
      {
        val id = run(es)(a).get
        run(es)(choices(id))
      }
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    es =>
      {
        val k = run(es)(a).get
        run(es)(f(k))
      }
  }

  def choiceViaFlatMap[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    flatMap(a)(a => if (a) ifTrue else ifFalse)
  }

  def choiceNViaFlatMap[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = {
    flatMap(a)(choices(_))
  }

  def choiceMapFlatMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] = {
    flatMap(a)(choices(_))
  }

  // Exercise 18
  def join[A](a: Par[Par[A]]): Par[A] = {
    es =>
      {
        val inner = run(es)(a).get
        run(es)(inner)
      }
  }

  def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(a)(f))
  }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = {
    flatMap(a)(x => x)
  }

  def main(args: Array[String]): Unit = {
    val par0 = Par.unit(List(4, 3, 2, 1))
    val executorService = Executors.newFixedThreadPool(2)

    // Exercise 3
    println("List -> Par: " + par0)
    println("List -> Future: UnitFuture: " + par0(executorService))
    println("List -> get: " + par0(executorService).get)
    println("List -> get: " + par0(executorService).get(0, TimeUnit.MILLISECONDS))
    println("Sort -> Par Lambda: ExecutorService => Future: " + Par.sortParViaMap(par0))
    println("Sort -> Future: Map2Future: " + Par.sortParViaMap(par0)(executorService))
    println("Sort -> get: " + Par.sortParViaMap(par0)(executorService).get)

    // Exercise 5
    println("map2ViaProduct: " + map2ViaProduct(par0, unit(()))((a, _) => a.sorted)(executorService).get)

    // Exercise 6
    println("parMap: " + Par.parMap(List(1, 2, 3, 4))(_ * 5))
    println("parMap: " + Par.parMap(List(1, 2, 3, 4))(_ * 5)(executorService))
    println("parMap: " + Par.parMap(List(1, 2, 3, 4))(_ * 5)(executorService).get)
    println("parMap: " + Par.run(executorService)(Par.parMap(List(1, 2, 3, 4))(_ * 5)))

    // Exercise 8
    println("parFilter: " + Par.run(executorService)(Par.parFilter(List(1, 2, 3, 4))(_ % 2 == 0)).get)
    println("parFilter2: " + Par.run(executorService)(Par.parFilter2(List(1, 2, 3, 4))(_ % 2 == 0)))

    println("equal: " + Par.equal(executorService)(map(unit(1))(_ + 1), unit(2)))

    val a = Par.lazyUnit(42 + 1)
    val s = Executors.newFixedThreadPool(1)
    // Deadlock. Pool of 1 thread, fork waiting for resource.
    // fork implementation uses two Callables.
    //println(Par.equal(s)(a, fork(a)))
    println(Par.equal(s)(a, delay(a)))

  }

}
package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  // Exercise 26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  // Exercise 27
  def depth[A](t: Tree[A]): Int = {
    def loop_depth(t: Tree[A], acc: Int): Int = t match {
      case Leaf(v) => acc
      case Branch(l, r) => loop_depth(l, acc + 1).max(loop_depth(r, acc + 1))
    }

    loop_depth(t, 0)
  }

  // Exercise 28
  def map[A, B](t: Tree[A])(f: (A) => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 29
  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 1)(_ + _)
  }

  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_.max(_))
  }

  /*def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 0)()
  }*/

  def main(args: Array[String]): Unit = {
    val tree0 = Leaf(1)
    val tree1 = Branch(Leaf(1), Leaf(2))
    val tree2 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    // Exercise 25
    println("size: " + size(tree0))
    println("size: " + size(tree1))
    println("size: " + size(tree2))

    // Exercise 26
    println("maximum: " + maximum(tree0))
    println("maximum: " + maximum(tree1))
    println("maximum: " + maximum(tree2))

    // Exercise 27
    println("depth: " + depth(tree0))
    println("depth: " + depth(tree1))
    println("depth: " + depth(tree2))

    // Exercise 28
    println("map: " + tree2)
    println("map: " + map(tree2)(_ + 1))
    println("map: " + map(tree2)(_ * 5))

    // Exercise 29
    println("fold: " + fold(tree2)(_ + 0)(_ + _))
    println("sizeViaFold: " + sizeViaFold(tree0))
    println("sizeViaFold: " + sizeViaFold(tree1))
    println("sizeViaFold: " + sizeViaFold(tree2))
    println("maximumViaFold: " + maximumViaFold(tree0))
    println("maximumViaFold: " + maximumViaFold(tree1))
    println("maximumViaFold: " + maximumViaFold(tree2))

  }
}
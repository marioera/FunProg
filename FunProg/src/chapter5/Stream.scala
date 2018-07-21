package chapter5

sealed trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
}

object Stream {

  def empty[A]: Stream[A] =
    new Stream[A] {
      def uncons = None
    }

  def cons[A](h: => A, t: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((h, t))
    }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    println("Stream")
  }

}
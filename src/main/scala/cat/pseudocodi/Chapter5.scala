package cat.pseudocodi

/**
  * @author fede
  */
object Chapter5 {

  sealed trait Stream[+A] {
    def drop(n: Int): Stream[A] = Empty

    def take(n: Int): Stream[A] = Empty

    def toList: List[A] = List[Nothing]()
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {

    override def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }

    override def drop(n: Int): Stream[A] = this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _ => this
    }

    override def toList: List[A] = List(h()) ::: t().toList
  }

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}

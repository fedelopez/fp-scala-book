package cat.pseudocodi

/**
  * @author fede
  */
object Chapter5 {

  sealed trait Stream[+A] {
    def toList: List[A]
  }

  case object Empty extends Stream[Nothing] {
    override def toList: List[Nothing] = List[Nothing]()
  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {

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

package ch2

/**
 * @author fede
 */
object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(x: List[Int]): Int = x match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  /**
   * What will be the result of the following expression?
   */
  def exercise31() = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(y, Cons(2, Cons(4, _))) => y
      case Nil => 42
      case Cons(y, Cons(z, Cons(3, Cons(4, _)))) => y + z
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    x
  }

}

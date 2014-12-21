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

    /**
     * exercise 3.2
     * Implement the function tail for removing the first element of the list.
     */
    def tail[A](x: List[A]): List[A] = x match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }

    /**
     * exercise 3.3
     * Implement the function setHead for replacing the first element of the list.
     */
    def setHead[A](x: List[A], newHead: A): List[A] = x match {
      case Nil => Cons(newHead, Nil)
      case Cons(xs, y) => Cons(newHead, tail(x))
    }

    /**
     * exercise 3.4
     * Generalise tail to the function drop, which removes the first n elements from a list. 
     */
    def drop[A](list: List[A], n: Int): List[A] = {
      def loop(x: List[A], idx: Int): List[A] = {
        if (idx == n) x
        else loop(tail(x), idx + 1)
      }
      loop(list, 0)
    }

    /**
     * exercise 3.5
     * Implement the function dropWhile, which removes elements from the list as long as they match a predicate.
     */
    def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
      case Nil => Nil
      case Cons(xs, y) => if (f(xs)) dropWhile(y, f) else Cons(xs, y)
    }
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

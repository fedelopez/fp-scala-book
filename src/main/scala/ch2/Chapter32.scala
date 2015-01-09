package ch2

/**
 * @author fede
 */
object Chapter32 {

  import ch2.Chapter3.List._
  import ch2.Chapter3._

  def sum(as: List[Int]): Int = {
    foldRight(as, 0)((x, y) => x + y)
  }

  def prod(as: List[Double]): Double = {
    foldRight(as, 1.0)((x, y) => x * y)
  }

  /**
   * exercise 3.9
   * Compute the length of a list using foldRight
   */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => 1 + length(tail(as)))
}

package cat.pseudocodi

/**
 * @author fede
 */
object Chapter2 {

  /**
   * Exercise 2.1
   */
  def fibonacci(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  /**
   * Exercise 2.2
   * Implement isSorted, which checks whether an array is sorted according to its given comparison function
   */
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(x: Array[A]): Boolean = {
      if (x.size < 2) true
      else {
        if (ordered(x.head, x.tail.head)) loop(x.tail)
        else false
      }
    }
    loop(arr)
  }

  /**
   * Exercise 2.3
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  /**
   * Exercise 2.4
   */
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a: A, b: B) => f(a)(b)

  /**
   * Exercise 2.5
   */
  def compose1[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def compose3[A, B, C](f: B => C, g: A => B): A => C = f compose g

  def compose2[A, B, C](f: B => C, g: A => B): A => C = g andThen f

}

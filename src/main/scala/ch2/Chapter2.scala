package ch2

/**
 * @author fede
 */
object Chapter2 {

  def fibonacci(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  /**
   * Implement isSorted, which checks whether an array is sorted according to its given comparison function
   */
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(x: Array[A]): Boolean = {
      if (x.size < 2) true
      else {
        val first: A = x.head
        val next: A = x.tail.head
        if (ordered(first, next)) loop(x.tail)
        else false
      }
    }
    loop(arr)
  }
}

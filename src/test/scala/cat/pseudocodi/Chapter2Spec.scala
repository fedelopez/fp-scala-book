package cat.pseudocodi

import cat.pseudocodi.Chapter2._
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author fede
 */
class Chapter2Spec extends FlatSpec with Matchers {

  it should "calculate the first fibonacci number" in {
    fibonacci(0) should be (0)
  }

  it should "calculate the second fibonacci number" in {
    fibonacci(1) should be (1)
  }

  it should "calculate the third fibonacci number" in {
    fibonacci(2) should be (1)
  }

  it should "calculate the nth fibonacci number" in {
    fibonacci(3) should be (2)
    fibonacci(4) should be (3)
    fibonacci(5) should be (5)
    fibonacci(6) should be (8)
    fibonacci(7) should be (13)
  }

  it should "calculate whether an empty array is sorted" in {
    val f: (Int, Int) => Boolean = _ < _
    assert(isSorted(Array(), f))
  }

  it should "calculate whether an array is sorted" in {
    val f: (Int, Int) => Boolean = _ < _
    assert(isSorted(Array(1, 2, 3), f))
  }

  it should "calculate whether an array is not sorted" in {
    val f: (Int, Int) => Boolean = _ < _
    assert(!isSorted(Array(3, 4, 1), f))
  }
}

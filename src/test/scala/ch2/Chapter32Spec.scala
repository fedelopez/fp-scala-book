package ch2

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author fede
 */
class Chapter32Spec extends FlatSpec with Matchers {

  it should "3.9 compute the length of a list using foldRight" in {
    Chapter32.length(Chapter3.List[Int](1, 2, 3, 4)) should be(4)
  }
}

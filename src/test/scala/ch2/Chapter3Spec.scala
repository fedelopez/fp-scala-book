package ch2

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author fede
 */
class Chapter3Spec extends FlatSpec with Matchers {

  it should "be 3 the result in exercise31" in {
    Chapter3.exercise31() should be(3)
  }
}

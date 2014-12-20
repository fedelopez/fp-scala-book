package ch2

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author fede
 */
class Chapter3Spec extends FlatSpec with Matchers {

  it should "be 3 the result in exercise31" in {
    Chapter3.exercise31() should be(3)
  }

  it should "3.2: should remove the first element in the list" in {
    val tail: Chapter3.List[Int] = Chapter3.List.tail(Chapter3.List(1, 2, 3, 4))
    tail should equal(Chapter3.List(2, 3, 4))
  }

  it should "3.2: should return empty list when no tail" in {
    val tail: Chapter3.List[Int] = Chapter3.List.tail(Chapter3.List())
    tail should equal(Chapter3.List())
  }

  it should "3.4: should set the first element in the list" in {
    val tail: Chapter3.List[Int] = Chapter3.List.setHead(Chapter3.List(1, 2, 3, 4), 6)
    tail should equal(Chapter3.List(6, 2, 3, 4))
  }

  it should "3.4: should set the first element in the empty list" in {
    val result: Chapter3.List[Int] = Chapter3.List.setHead(Chapter3.List(), 1)
    result should equal(Chapter3.List(1))
  }

  it should "3.4: should set the first element in the singleton list" in {
    val result: Chapter3.List[Int] = Chapter3.List.setHead(Chapter3.List(1), 2)
    result should equal(Chapter3.List(2))
  }
}

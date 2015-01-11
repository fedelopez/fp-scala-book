package ch2

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author fede
 */
class Chapter3Spec extends FlatSpec with Matchers {

  it should "3.1 be 3 the result" in {
    Chapter3.exercise31() should be(3)
  }

  it should "3.2 remove the first element in the list" in {
    val tail: Chapter3.List[Int] = Chapter3.List.tail(Chapter3.List(1, 2, 3, 4))
    tail should equal(Chapter3.List(2, 3, 4))
  }

  it should "3.2 return empty list when no tail" in {
    val tail: Chapter3.List[Int] = Chapter3.List.tail(Chapter3.List())
    tail should equal(Chapter3.List())
  }

  it should "3.3 set the first element in the list" in {
    val tail: Chapter3.List[Int] = Chapter3.List.setHead(Chapter3.List(1, 2, 3, 4), 6)
    tail should equal(Chapter3.List(6, 2, 3, 4))
  }

  it should "3.3 set the first element in the empty list" in {
    val result: Chapter3.List[Int] = Chapter3.List.setHead(Chapter3.List(), 1)
    result should equal(Chapter3.List(1))
  }

  it should "3.3 set the first element in the singleton list" in {
    val result: Chapter3.List[Int] = Chapter3.List.setHead(Chapter3.List(1), 2)
    result should equal(Chapter3.List(2))
  }

  it should "3.4 drop the first element in the list" in {
    val list: Chapter3.List[Int] = Chapter3.List(1, 2, 3, 4)
    val result: Chapter3.List[Int] = Chapter3.List.drop(list, 1)
    result should equal(Chapter3.List(2, 3, 4))
  }

  it should "3.4 drop the first and second elements in the list" in {
    val list: Chapter3.List[Int] = Chapter3.List(1, 2, 3, 4)
    val result: Chapter3.List[Int] = Chapter3.List.drop(list, 2)
    result should equal(Chapter3.List(3, 4))
  }

  it should "3.4 return empty list when drop empty list" in {
    val list: Chapter3.List[Int] = Chapter3.List()
    val result: Chapter3.List[Int] = Chapter3.List.drop(list, 2)
    result should equal(Chapter3.List())
  }

  it should "3.5 drop the first element in the list" in {
    val list: Chapter3.List[Int] = Chapter3.List(21, 20, 3, 4)
    val result: Chapter3.List[Int] = Chapter3.List.dropWhile(list, (value: Int) => value > 20)
    result should equal(Chapter3.List(20, 3, 4))
  }

  it should "3.5 not continue when function returns false" in {
    val list: Chapter3.List[Int] = Chapter3.List(5, 25, 3, 25)
    val result: Chapter3.List[Int] = Chapter3.List.dropWhile(list, (value: Int) => value % 5 == 0)
    result should equal(Chapter3.List(3, 25))
  }

  it should "3.5 drop all elements" in {
    val list: Chapter3.List[Int] = Chapter3.List(1, 2, 3, 4)
    val result: Chapter3.List[Int] = Chapter3.List.dropWhile(list, (value: Int) => value > 0)
    result should equal(Chapter3.List())
  }

  it should "3.6 return all the elements except the last one" in {
    val list: Chapter3.List[Int] = Chapter3.List(1, 2, 3, 4)
    val result: Chapter3.List[Int] = Chapter3.List.init(list)
    result should equal(Chapter3.List(1, 2, 3))
  }

  it should "3.9 compute the length of a list using foldRight" in {
    Chapter3.List.length(Chapter3.List[Int](1, 2, 3, 4)) should be(4)
  }

  it should "3.10 foldLeft" in {
    Chapter3.List.foldLeft(Chapter3.List[Int](1, 2, 3, 4), 0)(_ + _) should be(10)
  }
}

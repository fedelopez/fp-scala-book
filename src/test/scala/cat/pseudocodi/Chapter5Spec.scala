package cat.pseudocodi

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author fede
  */
class Chapter5Spec extends FlatSpec with Matchers {

  /**
    * exercise 5.1
    * Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL.
    * You can convert to the regular List type in the standard library. You can place this and other functions that operate on a Stream inside the Stream trait.
    */
  it should "5.1 convert a Stream to a List" in {
    val s: Chapter5.Stream[String] = Chapter5.Stream("apple", "microsoft", "gigabyte");
    s.toList should be(List("apple", "microsoft", "gigabyte"))
  }

  /**
    * exercise 5.2
    * Write the function take(n) for returning the first n elements of a Stream,
    */
  it should "return the first n elements of a Stream" in {
    val s: Chapter5.Stream[String] = Chapter5.Stream("apple", "microsoft", "gigabyte", "pivotal", "google")
    s.take(3).toList should be(List("apple", "microsoft", "gigabyte"))
  }

  /**
    * exercise 5.2
    * Write the function drop(n) for skipping the first n elements of a Stream,
    */
  it should "skip the first n elements of a Stream" in {
    val s: Chapter5.Stream[String] = Chapter5.Stream("apple", "microsoft", "gigabyte", "pivotal", "google")
    s.drop(0).toList should be(List("apple", "microsoft", "gigabyte", "pivotal", "google"))
    s.drop(3).toList should be(List("pivotal", "google"))
    s.drop(4).toList should be(List("google"))
  }

  /**
    * exercise 5.3
    * Write the function takeWhile for returning all starting elements of a stream that match the given predicate
    */
  it should "take elements starting with a" in {
    val s: Chapter5.Stream[String] = Chapter5.Stream("alaska", "atlanta", "gigabyte", "pivotal", "google")
    s.takeWhile((s: String) => s.startsWith("a")).toList should be(List("alaska", "atlanta"))
  }

  /**
    * exercise 5.4
    * Write the function forAll which checks that all elements match the given predicate
    */
  it should "hecks that all elements match the given predicate" in {
    val s: Chapter5.Stream[String] = Chapter5.Stream("alaska", "atlanta", "gigabyte", "pivotal", "tesla")
    s.forAll((s: String) => s.contains("a")) should be(true)
    s.forAll((s: String) => s.contains("l")) should be(false)
  }
}

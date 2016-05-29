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


  /**
    * exercise 5.2
    * Write the function drop(n) for skipping the first n elements of a Stream.
    */


}

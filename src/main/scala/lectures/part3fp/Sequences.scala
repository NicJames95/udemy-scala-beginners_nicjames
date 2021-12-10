package lectures.part3fp

import scala.util.Random

object Sequences extends App{

  /* Seq
    trait Seq[+A] {
    def head: A
    def tail: Seq[A]
    - A (very) general interface for data structures that
      - have a well defined order
      - can be indexed

    - Supports various operations:
      - apply, iterator, length, reverse for indexing and iterating
      - concatenation, appending, prepending
      - a lot of others: grouping, sorting, zipping, searching, slicing
  */
  // Seq
  val aSequence = Seq(1,3,2,4)
  println(aSequence)
  println(aSequence.reverse)
  println(aSequence(2)) // .apply, retrieves number at index 2
  println(aSequence ++ Seq(5,6,7))
  println(aSequence.sorted)

  // Ranges
  val aRange: Seq[Int] = 1 until 10
  aRange.foreach(println)

  (1 to 10).foreach(x => println("hello"))

  /* List
    sealed abstract class List[+A]
    case object Nil extends List[Nothing]
    case class ::[A](val hd: A, val tl: List[A]) extends List[A]

  - A LinearSeq immutable linked list
    - head, tail, isEmpty methods are fast: O(1)
    - most operations are O(n): length, reverse

  - Sealed - has two subtypes:
    - object Nil (empty)
    - class ::
  */
  // lists
  val aList = List(1,2,3)
  val prepended = 42 +: aList :+ 89// +: :: also prepends the list
  //  :+ does reverse
  println(prepended)

  val apples5 = List.fill(5)("apple")
  println(apples5)
  println(aList.mkString("-|-"))

  /* Array
    final class Array[T]
        extends java.io.Serializable
        with java.lang.Cloneable
    - The equivalent of simple Java arrays
      - can be manually constructed with predefined lengths
      - can be mutated(updated in place)
      - are interoperable with Java's T[] arrays
      - indexing is fast
  */
  // arrays
  val numbers = Array(1,2,3,4)
  val threeElements = Array.ofDim[Int](3)
  println(threeElements.foreach(println))

  // mutation
  numbers(2) = 0 // syntax sugar for numbers.update(2, 0)
  println(numbers.mkString(" "))

  // arrays and seq
  val numbersSeq: Seq[Int] = numbers // implicit conversion
  println(numbersSeq)

  /* Vector
    final class Vector[+A]
    - The default implementation for immutable sequences
      - effectively constant indexed read and write: O(log32(n))
      - fast element addition: append/prepend
      - implemented as a fixed-branched trie (branch factor 32)
      - good performance for large sizes

    val noElements = Vector.empty
    val numbers = noElements :+ 1 :+ 2 :+ 3     // Vector(1,2,3)
    val modified = numbers updated (0, 7)     // Vector(7,2,3)
  */

  // vectors
  val vector: Vector[Int] = Vector(1,2,3)
  println(vector)

  // vectors vs lists

  val maxRuns = 1000
  val maxCapacity = 1000000
  def getWriteTime(collection: Seq[Int]): Double = {
    val r = new Random() // instantiating new Random number generator
    val times = for {
      it <- 1 to maxRuns // for 1000 times or 1 to maxRuns
    } yield {
      val currentTime = System.nanoTime() // mark the current time
      collection.updated(r.nextInt(maxCapacity), r.nextInt())// update the collection at a random index with a random value
      System.nanoTime() - currentTime // measure the time after the collection is updated and then return time difference
    }
    times.sum * 1.0 / maxRuns // return the average

  }

  val numbersList = (1 to maxCapacity).toList
  val numbersVector = (1 to maxCapacity).toVector

  // keeps reference to tail
  // updating an element in the middle takes long
  println(getWriteTime(numbersList))
  // depth of the tree is small
  // needs to replace an entire 32-element chunk
  println(getWriteTime(numbersVector))


}

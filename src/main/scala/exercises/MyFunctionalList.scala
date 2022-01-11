package exercises

abstract class MyFunctionalList[+A] {

  /*
    head = first element of the list
    tail = remainder of the list
    isEmpty = is this list empty
    add(int) => new list with this element added
    toString => a string representation of the list
  
    2. transform the MyPredicate and MyTransformer into function types
  */

  def head: A
  def tail: MyFunctionalList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyFunctionalList[B]
  def printElements: String
  override def toString: String = "["+ printElements +"]"

  // higher-order functions
  def map[B](transformer: A => B): MyFunctionalList[B]
  def flatMap[B](transformer: A => MyFunctionalList[B]): MyFunctionalList[B]
  def filter(predicate: A => Boolean): MyFunctionalList[A]

  // concatenation
  def ++[B >: A](list: MyFunctionalList[B]): MyFunctionalList[B]

}

case object FunctionalEmpty extends MyFunctionalList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyFunctionalList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing] (element: B): MyFunctionalList[B] = new FunctionalCons(element, FunctionalEmpty)
  def printElements: String = ""

  def map[B](transformer: Nothing => B): MyFunctionalList[B] = FunctionalEmpty
  def flatMap[B](transformer: Nothing => MyFunctionalList[B]): MyFunctionalList[B] = FunctionalEmpty
  def filter(predicate: Nothing => Boolean): MyFunctionalList[Nothing] = FunctionalEmpty

  def ++[B >: Nothing](list: MyFunctionalList[B]): MyFunctionalList[B] = list
}

case class FunctionalCons[+A](h: A, t: MyFunctionalList[A]) extends MyFunctionalList[A] {
  def head: A = h
  def tail: MyFunctionalList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyFunctionalList[B] = new FunctionalCons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else s"$h ${t.printElements}"
  //  else h + " " + t.printElements

  /*
  [1,2,3].filter(n % 2 == 0) =
  [2,3].filter(n % 2 == 0) =
  = new FunctionalCons(2, [3].filter(n % 2 == 0))
  = new FunctionalCons(2, FunctionalEmpty.filter(n % 2 == 0))
  = new FunctionalCons(2, FunctionalEmpty)

  */
  def filter(predicate: A => Boolean): MyFunctionalList[A] =
    if(predicate(h)) new FunctionalCons(h, t.filter(predicate)) //(predicate.apply(h))
    else t.filter(predicate)

  /*
  [1,2,3].map(n * 2)
    = new FunctionalCons(2, [2,3].map(n * 2))
    = new FunctionalCons(2, new FunctionalCons(4, [3].map(n * 2)))
    = new FunctionalCons(2, new FunctionalCons(4, new FunctionalCons(6, FunctionalEmpty.map(n * 2))))
    = new FunctionalCons(2, new FunctionalCons(4, new FunctionalCons(6, FunctionalEmpty)))
  */
  def map[B](transformer: A => B): MyFunctionalList[B] =
    new FunctionalCons(transformer(h), t.map(transformer)) // (transformer.apply(h))

  /*
    [1,2] ++ [3,4,5]
    = new FunctionalCons(1, [2] ++ [3,4,5])
    = new FunctionalCons(1, new FunctionalCons(2, FunctionalEmpty ++ [3,4,5]))
    = new FunctionalCons(1, new FunctionalCons(2, new FunctionalCons(3, new FunctionalCons(4, new FunctionalCons(5)))))
  */
  def ++[B >: A](list: MyFunctionalList[B]): MyFunctionalList[B] = new FunctionalCons(h, t ++ list)

  /*
  [1,2].flatMap(n => [n, n+1])
  = [1,2] ++ [2].flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ FunctionalEmpty.flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ FunctionalEmpty
  = [1,2,2,3]
  */
  def flatMap[B](transformer: A => MyFunctionalList[B]): MyFunctionalList[B] =
    transformer(h) ++ t.flatMap(transformer) //transformer.apply(h)

}
/////// Replaced both functions by using functional programming calls 
//trait MyFunctionalPredicate[-T] { // T => Boolean
//  def test(elem: T): Boolean
//}

//trait MyFunctionalTransformer[-A, B] { // A => B
//  def transform(elem: A): B
//}

object FunctionalTest extends App {
  val listOfIntegers: MyFunctionalList[Int] = new FunctionalCons(1, new FunctionalCons(2, new FunctionalCons(3, FunctionalEmpty)))
  val cloneListOfIntegers: MyFunctionalList[Int] = new FunctionalCons(1, new FunctionalCons(2, new FunctionalCons(3, FunctionalEmpty)))
  val anotherListOfIntegers: MyFunctionalList[Int] = new FunctionalCons(4, new FunctionalCons(5, FunctionalEmpty))
  val listOfStrings: MyFunctionalList[String] = new FunctionalCons("Hello", new FunctionalCons("Scala", FunctionalEmpty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.map(new Function1[Int, Int] {
    override def apply(elem: Int): Int = elem * 2
  }).toString)

  println(listOfIntegers.filter(new Function1[Int, Boolean]{
    override def apply(elem: Int): Boolean = elem % 2 == 0
  }).toString)

  println((listOfIntegers ++ anotherListOfIntegers).toString)
  println(listOfIntegers.flatMap(new Function1[Int, MyFunctionalList[Int]] {
    override def apply(elem: Int): MyFunctionalList[Int] = new FunctionalCons(elem, new FunctionalCons(elem + 1, FunctionalEmpty))
  }).toString)

  println(cloneListOfIntegers == listOfIntegers)

}

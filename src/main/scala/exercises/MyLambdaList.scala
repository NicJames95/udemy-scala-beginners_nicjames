package exercises

abstract class MyLambdaList[+A] {

  /*
    head = first element of the list
    tail = remainder of the list
    isEmpty = is this list empty
    add(int) => new list with this element added
    toString => a string representation of the list
  */

  //1. MyFunctionalList: replace all FunctionX calls with lambdas

  def head: A
  def tail: MyLambdaList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyLambdaList[B]
  def printElements: String
  override def toString: String = "["+ printElements +"]"

  // higher-order functions
  def map[B](transformer: A => B): MyLambdaList[B]
  def flatMap[B](transformer: A => MyLambdaList[B]): MyLambdaList[B]
  def filter(predicate: A => Boolean): MyLambdaList[A]

  // concatenation
  def ++[B >: A](list: MyLambdaList[B]): MyLambdaList[B]

}

case object LambdaEmpty extends MyLambdaList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyLambdaList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing] (element: B): MyLambdaList[B] = new LambdaCons(element, LambdaEmpty)
  def printElements: String = ""

  def map[B](transformer: Nothing => B): MyLambdaList[B] = LambdaEmpty
  def flatMap[B](transformer: Nothing => MyLambdaList[B]): MyLambdaList[B] = LambdaEmpty
  def filter(predicate: Nothing => Boolean): MyLambdaList[Nothing] = LambdaEmpty

  def ++[B >: Nothing](list: MyLambdaList[B]): MyLambdaList[B] = list
}

case class LambdaCons[+A](h: A, t: MyLambdaList[A]) extends MyLambdaList[A] {
  def head: A = h
  def tail: MyLambdaList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyLambdaList[B] = new LambdaCons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else s"$h ${t.printElements}"
  //  else h + " " + t.printElements

  /*
  [1,2,3].filter(n % 2 == 0) =
  [2,3].filter(n % 2 == 0) =
  = new LambdaCons(2, [3].filter(n % 2 == 0))
  = new LambdaCons(2, LambdaEmpty.filter(n % 2 == 0))
  = new LambdaCons(2, LambdaEmpty)

  */
  def filter(predicate: A => Boolean): MyLambdaList[A] =
    if(predicate(h)) new LambdaCons(h, t.filter(predicate)) //(predicate.apply(h))
    else t.filter(predicate)

  /*
  [1,2,3].map(n * 2)
    = new LambdaCons(2, [2,3].map(n * 2))
    = new LambdaCons(2, new LambdaCons(4, [3].map(n * 2)))
    = new LambdaCons(2, new LambdaCons(4, new LambdaCons(6, LambdaEmpty.map(n * 2))))
    = new LambdaCons(2, new LambdaCons(4, new LambdaCons(6, LambdaEmpty)))
  */
  def map[B](transformer: A => B): MyLambdaList[B] =
    new LambdaCons(transformer(h), t.map(transformer)) // (transformer.apply(h))

  /*
    [1,2] ++ [3,4,5]
    = new LambdaCons(1, [2] ++ [3,4,5])
    = new LambdaCons(1, new LambdaCons(2, LambdaEmpty ++ [3,4,5]))
    = new LambdaCons(1, new LambdaCons(2, new LambdaCons(3, new LambdaCons(4, new LambdaCons(5)))))
  */
  def ++[B >: A](list: MyLambdaList[B]): MyLambdaList[B] = new LambdaCons(h, t ++ list)

  /*
  [1,2].flatMap(n => [n, n+1])
  = [1,2] ++ [2].flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ LambdaEmpty.flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ LambdaEmpty
  = [1,2,2,3]
  */
  def flatMap[B](transformer: A => MyLambdaList[B]): MyLambdaList[B] =
    transformer(h) ++ t.flatMap(transformer) //transformer.apply(h)

}

//trait MyFunctionalPredicate[-T] { // T => Boolean
//  def test(elem: T): Boolean
//}

//trait MyFunctionalTransformer[-A, B] { // A => B
//  def transform(elem: A): B
//}

object LambdaTest extends App {
  val listOfIntegers: MyLambdaList[Int] = new LambdaCons(1, new LambdaCons(2, new LambdaCons(3, LambdaEmpty)))
  val cloneListOfIntegers: MyLambdaList[Int] = new LambdaCons(1, new LambdaCons(2, new LambdaCons(3, LambdaEmpty)))
  val anotherListOfIntegers: MyLambdaList[Int] = new LambdaCons(4, new LambdaCons(5, LambdaEmpty))
  val listOfStrings: MyLambdaList[String] = new LambdaCons("Hello", new LambdaCons("Scala", LambdaEmpty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  //1. MyFunctionalList: replace all FunctionX calls with lambdas

  //  println(listOfIntegers.map(new Function1[Int, Int] {
  //    override def apply(elem: Int): Int = elem * 2
  //  }).toString)
  // replaces above with lambda expression function
  println(listOfIntegers.map(elem => elem * 2).toString)
  println(listOfIntegers.map(_ * 2).toString) // same ^^ as above


//  println(listOfIntegers.filter(new Function1[Int, Boolean]{
//    override def apply(elem: Int): Boolean = elem % 2 == 0
//  }).toString)
  println(listOfIntegers.filter(elem => elem % 2 == 0).toString)
  println(listOfIntegers.filter(_ % 2 == 0).toString) // same as ^^ above

  println((listOfIntegers ++ anotherListOfIntegers).toString)
//  println(listOfIntegers.flatMap(new Function1[Int, MyLambdaList[Int]] {
//    override def apply(elem: Int): MyLambdaList[Int] = new LambdaCons(elem, new LambdaCons(elem + 1, LambdaEmpty))
//  }).toString)
  println(listOfIntegers.flatMap(elem => new LambdaCons(elem, new LambdaCons(elem + 1, LambdaEmpty))).toString)
  // ^^ cannot use shortcut due to elem being used multiple times

  println(cloneListOfIntegers == listOfIntegers)

}

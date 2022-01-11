package exercises

abstract class MyCaseList[+A] {

  /*
    head = first element of the list
    tail = remainder of the list
    isEmpty = is this list empty
    add(int) => new list with this element added
    toString => a string representation of the list
  */

  /*
  Expand MyList - use classes and case objects
  */
  

  def head: A
  def tail: MyCaseList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyCaseList[B]
  def printElements: String
  override def toString: String = "["+ printElements +"]"

  def map[B](transformer: MyCaseTransformer[A, B]): MyCaseList[B]
  def flatMap[B](transformer: MyCaseTransformer[A, MyCaseList[B]]): MyCaseList[B]
  def filter(predicate: MyCasePredicate[A]): MyCaseList[A]

  // concatenation
  def ++[B >: A](list: MyCaseList[B]): MyCaseList[B]

}

case object CaseEmpty extends MyCaseList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyCaseList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing] (element: B): MyCaseList[B] = new CaseCons(element, CaseEmpty)
  def printElements: String = ""

  def map[B](transformer: MyCaseTransformer[Nothing, B]): MyCaseList[B] = CaseEmpty
  def flatMap[B](transformer: MyCaseTransformer[Nothing, MyCaseList[B]]): MyCaseList[B] = CaseEmpty
  def filter(predicate: MyCasePredicate[Nothing]): MyCaseList[Nothing] = CaseEmpty

  def ++[B >: Nothing](list: MyCaseList[B]): MyCaseList[B] = list
}

case class CaseCons[+A](h: A, t: MyCaseList[A]) extends MyCaseList[A] {
  def head: A = h
  def tail: MyCaseList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyCaseList[B] = new CaseCons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else s"$h ${t.printElements}"
  //  else h + " " + t.printElements

  /*
  [1,2,3].filter(n % 2 == 0) =
  [2,3].filter(n % 2 == 0) =
  = new CaseCons(2, [3].filter(n % 2 == 0))
  = new CaseCons(2, CaseEmpty.filter(n % 2 == 0))
  = new CaseCons(2, CaseEmpty)

  */
  def filter(predicate: MyCasePredicate[A]): MyCaseList[A] =
    if(predicate.test(h)) new CaseCons(h, t.filter(predicate))
    else t.filter(predicate)

  /*
  [1,2,3].map(n * 2)
    = new CaseCons(2, [2,3].map(n * 2))
    = new CaseCons(2, new CaseCons(4, [3].map(n * 2)))
    = new CaseCons(2, new CaseCons(4, new CaseCons(6, CaseEmpty.map(n * 2))))
    = new CaseCons(2, new CaseCons(4, new CaseCons(6, CaseEmpty)))
  */
  def map[B](transformer: MyCaseTransformer[A, B]): MyCaseList[B] =
    new CaseCons(transformer.transform(h), t.map(transformer))

  /*
    [1,2] ++ [3,4,5]
    = new CaseCons(1, [2] ++ [3,4,5])
    = new CaseCons(1, new CaseCons(2, CaseEmpty ++ [3,4,5]))
    = new CaseCons(1, new CaseCons(2, new CaseCons(3, new CaseCons(4, new CaseCons(5)))))
  */
  def ++[B >: A](list: MyCaseList[B]): MyCaseList[B] = new CaseCons(h, t ++ list)

  /*
  [1,2].flatMap(n => [n, n+1])
  = [1,2] ++ [2].flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ CaseEmpty.flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ CaseEmpty
  = [1,2,2,3]
  */
  def flatMap[B](transformer: MyCaseTransformer[A, MyCaseList[B]]): MyCaseList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)

}

trait MyCasePredicate[-T] {
  def test(elem: T): Boolean
}

trait MyCaseTransformer[-A, B] {
  def transform(elem: A): B
}

object CaseTest extends App {
  val listOfIntegers: MyCaseList[Int] = new CaseCons(1, new CaseCons(2, new CaseCons(3, CaseEmpty)))
  val cloneListOfIntegers: MyCaseList[Int] = new CaseCons(1, new CaseCons(2, new CaseCons(3, CaseEmpty)))
  val anotherListOfIntegers: MyCaseList[Int] = new CaseCons(4, new CaseCons(5, CaseEmpty))
  val listOfStrings: MyCaseList[String] = new CaseCons("Hello", new CaseCons("Scala", CaseEmpty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.map(new MyCaseTransformer[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }).toString)

  println(listOfIntegers.filter(new MyCasePredicate[Int]{
    override def test(elem: Int): Boolean = elem % 2 == 0
  }).toString)

  println((listOfIntegers ++ anotherListOfIntegers).toString)
  println(listOfIntegers.flatMap(new MyCaseTransformer[Int, MyCaseList[Int]] {
    override def transform(elem: Int): MyCaseList[Int] = new CaseCons(elem, new CaseCons(elem + 1, CaseEmpty))
  }).toString)

  println(cloneListOfIntegers == listOfIntegers)

}

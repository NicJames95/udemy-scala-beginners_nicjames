package exercises
//expand MyList to be generic
abstract class MyCovariantList[+A] {

  /*
    head = first element of the list
    tail = remainder of the list
    isEmpty = is this list empty
    add(int) => new list with this element added
    toString => a string representation of the list
  
3. MyList:
      - map(transformer) => MyList
      - filter(predicate) => MyList
      - flatMap(transformer from A to MyList[B]) => MyList[B]

      class EvenPredicate extends MyPredicate[Int]
      class StringToIntTransformer extends MyTransformer[String, Int]

      [1,2,3].map(n * 2) = [2,4,6]
      [1,2,3,4].filter(n % 2) = [2,4]
      [1,2,3].flatMap(n => [n, n+1]) => [1,2,2,3,3,4]
  */
  

  //expand MyList to be generic

  def head: A
  def tail: MyCovariantList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyCovariantList[B]
  def printElements: String
  override def toString: String = "["+ printElements +"]"

  def map[B](transformer: MyTransformer[A, B]): MyCovariantList[B]
  def flatMap[B](transformer: MyTransformer[A, MyCovariantList[B]]): MyCovariantList[B]
  def filter(predicate: MyPredicate[A]): MyCovariantList[A]

  // concatenation
  def ++[B >: A](list: MyCovariantList[B]): MyCovariantList[B]

}

object CovariantEmpty extends MyCovariantList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyCovariantList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing] (element: B): MyCovariantList[B] = new CovariantCons(element, CovariantEmpty)
  def printElements: String = ""

  def map[B](transformer: MyTransformer[Nothing, B]): MyCovariantList[B] = CovariantEmpty
  def flatMap[B](transformer: MyTransformer[Nothing, MyCovariantList[B]]): MyCovariantList[B] = CovariantEmpty
  def filter(predicate: MyPredicate[Nothing]): MyCovariantList[Nothing] = CovariantEmpty

  def ++[B >: Nothing](list: MyCovariantList[B]): MyCovariantList[B] = list
}

class CovariantCons[+A](h: A, t: MyCovariantList[A]) extends MyCovariantList[A] {
  def head: A = h
  def tail: MyCovariantList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyCovariantList[B] = new CovariantCons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else s"$h ${t.printElements}"
//  else h + " " + t.printElements

  /*
  [1,2,3].filter(n % 2 == 0) =
  [2,3].filter(n % 2 == 0) =
  = new CovariantCons(2, [3].filter(n % 2 == 0))
  = new CovariantCons(2, CovariantEmpty.filter(n % 2 == 0))
  = new CovariantCons(2, CovariantEmpty)

  */
  def filter(predicate: MyPredicate[A]): MyCovariantList[A] =
    if(predicate.test(h)) new CovariantCons(h, t.filter(predicate))
    else t.filter(predicate)

  /*
  [1,2,3].map(n * 2)
    = new CovariantCons(2, [2,3].map(n * 2))
    = new CovariantCons(2, new CovariantCons(4, [3].map(n * 2)))
    = new CovariantCons(2, new CovariantCons(4, new CovariantCons(6, CovariantEmpty.map(n * 2))))
    = new CovariantCons(2, new CovariantCons(4, new CovariantCons(6, CovariantEmpty)))
  */
  def map[B](transformer: MyTransformer[A, B]): MyCovariantList[B] =
    new CovariantCons(transformer.transform(h), t.map(transformer))

  /*
    [1,2] ++ [3,4,5]
    = new CovariantCons(1, [2] ++ [3,4,5])
    = new CovariantCons(1, new CovariantCons(2, CovariantEmpty ++ [3,4,5]))
    = new CovariantCons(1, new CovariantCons(2, new CovariantCons(3, new CovariantCons(4, new CovariantCons(5)))))
  */
  def ++[B >: A](list: MyCovariantList[B]): MyCovariantList[B] = new CovariantCons(h, t ++ list)

  /*
  [1,2].flatMap(n => [n, n+1])
  = [1,2] ++ [2].flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ CovariantEmpty.flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ CovariantEmpty
  = [1,2,2,3]
  */
  def flatMap[B](transformer: MyTransformer[A, MyCovariantList[B]]): MyCovariantList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)

}

trait MyPredicate[-T] {
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(elem: A): B
}

object CovariantListTest extends App {
  val listOfIntegers: MyCovariantList[Int] = new CovariantCons(1, new CovariantCons(2, new CovariantCons(3, CovariantEmpty)))
  val anotherListOfIntegers: MyCovariantList[Int] = new CovariantCons(4, new CovariantCons(5, CovariantEmpty))
  val listOfStrings: MyCovariantList[String] = new CovariantCons("Hello", new CovariantCons("Scala", CovariantEmpty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.map(new MyTransformer[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }).toString)

  println(listOfIntegers.filter(new MyPredicate[Int]{
    override def test(elem: Int): Boolean = elem % 2 == 0
  }).toString)

  println((listOfIntegers ++ anotherListOfIntegers).toString)
  println(listOfIntegers.flatMap(new MyTransformer[Int, MyCovariantList[Int]] {
    override def transform(elem: Int): MyCovariantList[Int] = new CovariantCons(elem, new CovariantCons(elem + 1, CovariantEmpty))
  }).toString)


}

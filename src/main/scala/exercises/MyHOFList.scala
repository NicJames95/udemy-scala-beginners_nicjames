package exercises

abstract class MyHOFList[+A] {

  /*
    head = first element of the list
    tail = remainder of the list
    isEmpty = is this list empty
    add(int) => new list with this element added
    toString => a string representation of the list
  */

/*  1. Expand MyList
    - foreach method A => Unit
    [1,2,3].foreach(x => println(x))

    - sort function ((A, A) => Int) => MyList
    [1,2,3].sort((x,y) => y - x) => [3,2,1]

    - zipWith (list, (A, A) => B) => MyList[B]
    [1,2,3].zipWith([4,5,6], x * y) => [1 * 4, 2 * 5, 3 * 6] = [4,10,18]

    - fold(start)(function) => a value
    [1,2,3].fold(0)(x + y) = 6
*/

  def head: A
  def tail: MyHOFList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyHOFList[B]
  def printElements: String
  override def toString: String = "["+ printElements +"]"

  // higher-order functions
  def map[B](transformer: A => B): MyHOFList[B]
  def flatMap[B](transformer: A => MyHOFList[B]): MyHOFList[B]
  def filter(predicate: A => Boolean): MyHOFList[A]


  // concatenation
  def ++[B >: A](list: MyHOFList[B]): MyHOFList[B]

  // hofs
  def foreach(f: A => Unit): Unit
  def sort(compare: (A, A) => Int): MyHOFList[A]
  def zipWith[B, C](list: MyHOFList[B], zip:(A, B) => C): MyHOFList[C]
  def fold[B](start: B)(operator: (B, A) => B): B

}

case object HOFEmpty extends MyHOFList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyHOFList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing] (element: B): MyHOFList[B] = new HOFCons(element, HOFEmpty)
  def printElements: String = ""

  def map[B](transformer: Nothing => B): MyHOFList[B] = HOFEmpty
  def flatMap[B](transformer: Nothing => MyHOFList[B]): MyHOFList[B] = HOFEmpty
  def filter(predicate: Nothing => Boolean): MyHOFList[Nothing] = HOFEmpty

  def ++[B >: Nothing](list: MyHOFList[B]): MyHOFList[B] = list

  // hofs
  def foreach(f: Nothing => Unit): Unit = ()
  def sort(compare: (Nothing, Nothing) => Int) = HOFEmpty
  def zipWith[B, C](list: MyHOFList[B], zip: (Nothing, B) => C): MyHOFList[C] =
    if(!list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else HOFEmpty
  def fold[B](start: B)(operator: (B, Nothing) => B): B = start

}

case class HOFCons[+A](h: A, t: MyHOFList[A]) extends MyHOFList[A] {
  def head: A = h
  def tail: MyHOFList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyHOFList[B] = new HOFCons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else s"$h ${t.printElements}"
  //  else h + " " + t.printElements

  /*
  [1,2,3].filter(n % 2 == 0) =
  [2,3].filter(n % 2 == 0) =
  = new HOFCons(2, [3].filter(n % 2 == 0))
  = new HOFCons(2, HOFEmpty.filter(n % 2 == 0))
  = new HOFCons(2, HOFEmpty)

  */
  def filter(predicate: A => Boolean): MyHOFList[A] =
    if(predicate(h)) new HOFCons(h, t.filter(predicate)) //(predicate.apply(h))
    else t.filter(predicate)

  /*
  [1,2,3].map(n * 2)
    = new HOFCons(2, [2,3].map(n * 2))
    = new HOFCons(2, new HOFCons(4, [3].map(n * 2)))
    = new HOFCons(2, new HOFCons(4, new HOFCons(6, HOFEmpty.map(n * 2))))
    = new HOFCons(2, new HOFCons(4, new HOFCons(6, HOFEmpty)))
  */
  def map[B](transformer: A => B): MyHOFList[B] =
    new HOFCons(transformer(h), t.map(transformer)) // (transformer.apply(h))

  /*
    [1,2] ++ [3,4,5]
    = new HOFCons(1, [2] ++ [3,4,5])
    = new HOFCons(1, new HOFCons(2, HOFEmpty ++ [3,4,5]))
    = new HOFCons(1, new HOFCons(2, new HOFCons(3, new HOFCons(4, new HOFCons(5)))))
  */
  def ++[B >: A](list: MyHOFList[B]): MyHOFList[B] = new HOFCons(h, t ++ list)

  /*
  [1,2].flatMap(n => [n, n+1])
  = [1,2] ++ [2].flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ HOFEmpty.flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ HOFEmpty
  = [1,2,2,3]
  */
  def flatMap[B](transformer: A => MyHOFList[B]): MyHOFList[B] =
    transformer(h) ++ t.flatMap(transformer) //transformer.apply(h)

  // hofs
  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def sort(compare: (A,A) => Int): MyHOFList[A] = {
    def insert(x: A, sortedList: MyHOFList[A]): MyHOFList[A] =
      if (sortedList.isEmpty) new HOFCons(x, HOFEmpty)
      else if (compare(x, sortedList.head) <= 0) new HOFCons(x, sortedList)
      // if x is less than the smallest element in the list x will be the new smallest element
      else new HOFCons(sortedList.head, insert(x, sortedList.tail))
    val sortedTail = t.sort(compare)
    insert(h, sortedTail)
  }

  def zipWith[B, C](list: MyHOFList[B], zip: (A, B) => C): MyHOFList[C] =
    if(list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else new HOFCons(zip(h, list.head), t.zipWith(list.tail, zip))

  /*
  [1,2,3].fold(0)(+) =
  = [2,3].fold(1)(+) =
  = [3].fold(3)(+) =
  = [].fold(6)(+)
  = 6
  */
  def fold[B](start: B)(operator: (B, A) => B): B = {
    t.fold(operator(start, h))(operator)
  }

}

//trait MyFunctionalPredicate[-T] { // T => Boolean
//  def test(elem: T): Boolean
//}

//trait MyFunctionalTransformer[-A, B] { // A => B
//  def transform(elem: A): B
//}

object HOFTest extends App {
  val listOfIntegers: MyHOFList[Int] = new HOFCons(1, new HOFCons(2, new HOFCons(3, HOFEmpty)))
  val cloneListOfIntegers: MyHOFList[Int] = new HOFCons(1, new HOFCons(2, new HOFCons(3, HOFEmpty)))
  val anotherListOfIntegers: MyHOFList[Int] = new HOFCons(4, new HOFCons(5, HOFEmpty))
  val listOfStrings: MyHOFList[String] = new HOFCons("Hello", new HOFCons("Scala", HOFEmpty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

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
  println(listOfIntegers.flatMap(elem => new HOFCons(elem, new HOFCons(elem + 1, HOFEmpty))).toString)
  // ^^ cannot use shortcut due to elem being used multiple times

  println(cloneListOfIntegers == listOfIntegers)

  // hofs
  listOfIntegers.foreach(println)
  println(listOfIntegers.sort((x, y) => y - x))
  println(anotherListOfIntegers.zipWith[String, String](listOfStrings, _ + "-" + _))
  println(listOfIntegers.fold(0)(_ + _))

}

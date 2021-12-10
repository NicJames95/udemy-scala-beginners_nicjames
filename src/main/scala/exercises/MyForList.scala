package exercises

abstract class MyForList[+A] {

  /*
    head = first element of the list
    tail = remainder of the list
    isEmpty = is this list empty
    add(int) => new list with this element added
    toString => a string representation of the list


    1. MyList supports for comprehensions?
       map(f: A => B) => MyList[B]
       filter(p: A => Boolean) => MyList[A]
       flatMap(f: A => MyList[B]) => MyList[B]
  */

  def head: A
  def tail: MyForList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyForList[B]
  def printElements: String
  override def toString: String = "["+ printElements +"]"

  // higher-order functions
  def map[B](transformer: A => B): MyForList[B]
  def flatMap[B](transformer: A => MyForList[B]): MyForList[B]
  def filter(predicate: A => Boolean): MyForList[A]


  // concatenation
  def ++[B >: A](list: MyForList[B]): MyForList[B]

  // hofs
  def foreach(f: A => Unit): Unit
  def sort(compare: (A, A) => Int): MyForList[A]
  def zipWith[B, C](list: MyForList[B], zip:(A, B) => C): MyForList[C]
  def fold[B](start: B)(operator: (B, A) => B): B

}

case object ForEmpty extends MyForList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyForList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing] (element: B): MyForList[B] = new ForCons(element, ForEmpty)
  def printElements: String = ""

  def map[B](transformer: Nothing => B): MyForList[B] = ForEmpty
  def flatMap[B](transformer: Nothing => MyForList[B]): MyForList[B] = ForEmpty
  def filter(predicate: Nothing => Boolean): MyForList[Nothing] = ForEmpty

  def ++[B >: Nothing](list: MyForList[B]): MyForList[B] = list

  // hofs
  def foreach(f: Nothing => Unit): Unit = ()
  def sort(compare: (Nothing, Nothing) => Int) = ForEmpty
  def zipWith[B, C](list: MyForList[B], zip: (Nothing, B) => C): MyForList[C] =
    if(!list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else ForEmpty
  def fold[B](start: B)(operator: (B, Nothing) => B): B = start

}

case class ForCons[+A](h: A, t: MyForList[A]) extends MyForList[A] {
  def head: A = h
  def tail: MyForList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyForList[B] = new ForCons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else s"$h ${t.printElements}"
  //  else h + " " + t.printElements

  /*
  [1,2,3].filter(n % 2 == 0) =
  [2,3].filter(n % 2 == 0) =
  = new ForCons(2, [3].filter(n % 2 == 0))
  = new ForCons(2, ForEmpty.filter(n % 2 == 0))
  = new ForCons(2, ForEmpty)

  */
  def filter(predicate: A => Boolean): MyForList[A] =
    if(predicate(h)) new ForCons(h, t.filter(predicate)) //(predicate.apply(h))
    else t.filter(predicate)

  /*
  [1,2,3].map(n * 2)
    = new ForCons(2, [2,3].map(n * 2))
    = new ForCons(2, new ForCons(4, [3].map(n * 2)))
    = new ForCons(2, new ForCons(4, new ForCons(6, ForEmpty.map(n * 2))))
    = new ForCons(2, new ForCons(4, new ForCons(6, ForEmpty)))
  */
  def map[B](transformer: A => B): MyForList[B] =
    new ForCons(transformer(h), t.map(transformer)) // (transformer.apply(h))

  /*
    [1,2] ++ [3,4,5]
    = new ForCons(1, [2] ++ [3,4,5])
    = new ForCons(1, new ForCons(2, ForEmpty ++ [3,4,5]))
    = new ForCons(1, new ForCons(2, new ForCons(3, new ForCons(4, new ForCons(5)))))
  */
  def ++[B >: A](list: MyForList[B]): MyForList[B] = new ForCons(h, t ++ list)

  /*
  [1,2].flatMap(n => [n, n+1])
  = [1,2] ++ [2].flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ ForEmpty.flatMap(n => [n, n+1])
  = [1,2] ++ [2,3] ++ ForEmpty
  = [1,2,2,3]
  */
  def flatMap[B](transformer: A => MyForList[B]): MyForList[B] =
    transformer(h) ++ t.flatMap(transformer) //transformer.apply(h)

  // hofs
  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def sort(compare: (A,A) => Int): MyForList[A] = {
    def insert(x: A, sortedList: MyForList[A]): MyForList[A] =
      if (sortedList.isEmpty) new ForCons(x, ForEmpty)
      else if (compare(x, sortedList.head) <= 0) new ForCons(x, sortedList)
      // if x is less than the smallest element in the list x will be the new smallest element
      else new ForCons(sortedList.head, insert(x, sortedList.tail))
    val sortedTail = t.sort(compare)
    insert(h, sortedTail)
  }

  def zipWith[B, C](list: MyForList[B], zip: (A, B) => C): MyForList[C] =
    if(list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else new ForCons(zip(h, list.head), t.zipWith(list.tail, zip))

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

object ForTest extends App {
  val listOfIntegers: MyForList[Int] = new ForCons(1, new ForCons(2, new ForCons(3, ForEmpty)))
  val cloneListOfIntegers: MyForList[Int] = new ForCons(1, new ForCons(2, new ForCons(3, ForEmpty)))
  val anotherListOfIntegers: MyForList[Int] = new ForCons(4, new ForCons(5, ForEmpty))
  val listOfStrings: MyForList[String] = new ForCons("Hello", new ForCons("Scala", ForEmpty))

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
  println(listOfIntegers.flatMap(elem => new ForCons(elem, new ForCons(elem + 1, ForEmpty))).toString)
  // ^^ cannot use shortcut due to elem being used multiple times

  println(cloneListOfIntegers == listOfIntegers)

  // hofs
  listOfIntegers.foreach(println)
  println(listOfIntegers.sort((x, y) => y - x))
  println(anotherListOfIntegers.zipWith[String, String](listOfStrings, _ + "-" + _))
  println(listOfIntegers.fold(0)(_ + _))

  // for comprehensions
  val combinations = for {
    n <- listOfIntegers
    string <- listOfStrings
  } yield n + "-" + string
  println(combinations)
}

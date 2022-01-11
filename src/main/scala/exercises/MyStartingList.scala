package exercises

abstract class MyStartingList {

  /*
    head = first element of the list
    tail = remainder of the list
    isEmpty = is this list empty
    add(int) => new list with this element added
    toString => a string representation of the list
  */

  def head: Int
  def tail: MyStartingList
  def isEmpty: Boolean
  def add(element: Int): MyStartingList
  def printElements: String
  // polymorphic call
  override def toString: String = "[" + printElements + "]"

}

object StartingEmpty extends MyStartingList {
  def head: Int = throw new NoSuchElementException
  def tail: MyStartingList = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add(element: Int): MyStartingList = new StartingCons(element, StartingEmpty)
  def printElements: String = ""
}

class StartingCons(h: Int, t: MyStartingList) extends MyStartingList {
  def head: Int = h
  def tail: MyStartingList = t
  def isEmpty: Boolean = false
  def add(element: Int): MyStartingList = new StartingCons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else h + " " + t.printElements
}

object StartingListTest extends App {
  val list = new StartingCons(1, new StartingCons(2, new StartingCons(3, StartingEmpty)))
  println(list.tail.head) // prints 2 goes to tail then to next head element
  println(list.add(4).head)
  println(list.isEmpty)

  println(list.toString)
  

}
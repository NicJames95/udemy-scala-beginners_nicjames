package lectures.part2oop

object Generics extends App {

  class MyList[+A] {
    // use the type A inside class definition
    def add[B >: A](element: B): MyList[B] = ???
    // takes a parameter of type B which is super type of A
    /*
    A = Cat
    B = Dog = Animal
    list changes to a list of animals
    */
  }

  class MyMap[Key, Value]

  val listOfIntegers = new MyList[Int]
  val listOfStrings = new MyList[String]

  // generic methods
  object MyList {
    def empty[A]: MyList[A] = ???
  }
  val emptyListOfIntegers = MyList.empty[Int]

  // variance problem
  class Animal
  class Cat extends Animal
  class Dog extends Animal

  // 1. yes List[Cat] extends List[Animal] = COVARIANCE
  class CovariantList[+A]
  val animal: Animal = new Cat
  val animalList: CovariantList[Animal] = new CovariantList[Cat]
  // animalList.add(new Dog) ??? HARD QUESTION
  // adding a dog to the list will change the list to a list of animals

  // 2. NO = INVARIANCE
  class InvariantList[A]
  val invariantAnimalList: InvariantList[Animal] = new InvariantList[Animal]

  // 3. Hell, no! CONTRAVARIANCE
  class ContravariantList[-A]
  val contravariantList: ContravariantList[Cat] = new ContravariantList[Animal]

  class Trainer[-A]
  val trainer: Trainer[Cat] = new Trainer[Animal]

  // bounded types
  class Cage[A <: Animal] (animal: A) // class Cage only accepts subtypes parameter of Animal
  val cage = new Cage(new Dog)

  //  class Car
  // generic type needs proper bounded type 
  //  val newCage = new Cage(new Car)

  //expand MyList to be generic

}

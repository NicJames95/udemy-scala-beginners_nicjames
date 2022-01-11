package lectures.part2oop

object CaseClasses extends App{

  /*
  equals, hashCode, toString case classes provide shorthand for these companion object
  */

  case class Person(name: String, age: Int)

  // 1. class parameters are fields
  val jim = new Person("Jim", 34)
  println(jim.name) // valid due to case class

  // 2. sensible toString
  // without case prints object reference (lectures.part2oop.CaseClasses$Person@7a3d45bd)
  // with case prints (Person(Jim,34))
  // println(jim.toString)
  // also prints jim toString = println(instance) = println(instance.toString) // syntactic sugar
  println(jim)

  // 3. equals and hashCode implemented OOTB(out of the box)
  val jim2 = new Person("Jim", 34)
  println(jim == jim2)

  // 4. CCs(case classes) have handy copy method
  val jim3 = jim.copy(age = 45)
  println(jim3)

  // 5. CCs have companion objects
  val thePerson = Person
  // apply method
  val mary = Person("Mary", 23)

  // 6. CCs are serializeable
  // Akka

  // 7. CCs have extractor patterns = CCs can be used in PATTERN MATCHING
  
  case object UnitedKingdom {
    def name: String = "The UK of GB and NI"
  }

  /*
  Expand MyList - use classes and case objects
  */
}

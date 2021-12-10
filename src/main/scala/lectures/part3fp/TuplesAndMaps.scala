package lectures.part3fp

object TuplesAndMaps extends App {

  // tuples = finite ordered "lists"
  //val aTuple = new Tuple2(2, "hello, Scala") // Tuple2[Int, String] = (Int, String)
  // or
  /* Tuples
  val tuple = (42, "RockTheJVM")
  tuple._1    // 42
  tuple.copy(_1 = 0)    // (0, RockTheJVM)
  tuple.toString    // "(42, RockTheJVM)"
  tuple.swap    // (RockTheJVM, 42)
  */
  val aTuple = (2, "hello, Scala")

  println(aTuple._1) // prints 2 to the console
  println(aTuple.copy(_2 = "goodbye Java"))
  println(aTuple.swap) // ("hello, Scala", 2)

  // Maps - keys -> values
  /* Maps
  val phonebook = Map("Jim -> 555, "Mary -> 789)
  phonebook.contains("Jim")
  val anotherbook = phonebook + ("Daniel", 123)
  */
  val aMap: Map[String, Int] = Map()

  val phonebook = Map(("Jim", 555), ("Daniel" -> 789)).withDefaultValue(-1) // adds safeguard for NoSuchElementException
  // a -> b is sugar for (a,b)
  println(phonebook)

  // map ops
  println(phonebook.contains("Jim"))
  println(phonebook("Jim")) // .apply method to return the value associated with key "Jim"

  // add a pairing
  val newPairing = "Mary" -> 678
  val newPhonebook = phonebook + newPairing
  println(newPhonebook)

  // functionals on maps
  // map, flatMap, filter

  println(phonebook.map(pair => pair._1.toLowerCase -> pair._2)) // changes to lowercase

  // filterKeys
  // API has changed since Scala 2.13 use phonebook.view.filterKeys(...).toMap
  //println(phonebook.filterKeys(x => x.startsWith("J"))) // prints where keys are filtered by predicate startsWith "J"
  println(phonebook.view.filterKeys(x => x.startsWith("J")).toMap)
  // mapValues
  // API has changed use phonebook.view.mapValues(number => number * 10).toMap)
  // println(phonebook.mapValues(number => number * 10)) // goes through all the values and appends a 0 to them
  println(phonebook.view.mapValues(number => "0245-" + number * 10).toMap)

  // conversions to other collections
  println(phonebook.toList)
  println(List(("Daniel", 555)).toMap)
  val names = List("Bob", "James", "Angela", "Mary", "Daniel", "Jim")
  println(names.groupBy(name => name.charAt(0)))

  /*
  1. What would happen if I had two original entries "Jim" -> 555 and "JIM" -> 900?

    !!! careful with mapping keys as they could overlap causing loss of data e.g above example ^^^

  2. Overly simplified social network based on maps
      Person = String
      - add a person to the network
      - remove
      - friend(mutual)
      - unfriend

      - number of friends of a person
      - person with most friends
      - how many people have NO friends
      - if there is a social connection between two people (direct or not)
  */

  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    network + (person -> Set())

  def friend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)

//    network + (a -> (friendsA :+ b)) + (b -> (friendsB :+ a)) how to add to the lists now its a set
// using set also guarantees that each person set is unique
  network + (a -> (friendsA + b)) + (b -> (friendsB + a))
  }

  def unfriend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)

    network + (a -> (friendsA - b)) + (b -> (friendsB - a))
  }

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]): Map[String, Set[String]] =
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(networkAcc, person, friends.head))

    val unfriended = removeAux(network(person), network)
    unfriended - person
  }

  val empty: Map[String, Set[String]] = Map()
  val network = add(add(empty,"Bob"), "Mary")
  println(network)
  println(friend(network, "Bob", "Mary"))
  println(unfriend(friend(network,"Bob", "Mary"), "Bob", "Mary"))
  println(remove(friend(network, "Bob", "Mary"), "Bob"))

  // Jim,Bob,Mary   Bob & Mary are friends, Bob & Jim are friends, Jim & Mary are not
  val people = add(add(add(empty, "Bob"), "Mary"), "Jim")
  val jimBob = friend(people, "Bob", "Jim")
  val maryBob = friend(people, "Bob", "Mary")
  val testNet = friend(jimBob, "Bob", "Mary")

  println(testNet)

  def nFriends(network: Map[String, Set[String]], person: String): Int =
    if(!network.contains(person)) 0
    else network(person).size

  println(nFriends(testNet, "Bob"))

  def mostFriends(network: Map[String, Set[String]]): String =
    network.maxBy(pair => pair._2.size)._1 // pair._2 = list of friends for every pairing ._1 = key or first element of that pairing
  println(mostFriends(testNet))

  def nPeopleWithNoFriends(network: Map[String, Set[String]]): Int =
    network.filterKeys(k => network(k).isEmpty).size // filters through the list for their keys/friends list is empty
  // then .size to get the total number
  // could also filter pairings     network.filter(pair => pair._2.isEmpty).size
  // also could use count method    network.count(pair => pair._2.isEmpty)
  // even more shorthanded    network.count(_._2.isEmpty)
  println(nPeopleWithNoFriends(testNet))

  def socialConnection(network: Map[String, Set[String]], a: String, b: String): Boolean = {
    def bfs(target: String, consideredPeople: Set[String], discoveredPeople: Set[String]): Boolean = {
      if (discoveredPeople.isEmpty) false
      else {
        val person = discoveredPeople.head
        if (person == target) true
        else if (consideredPeople.contains(person)) bfs(target, consideredPeople, discoveredPeople.tail)
        else bfs(target, consideredPeople + person, discoveredPeople.tail ++ network(person))
      }
    }
    bfs(b, Set(), network(a) + a) // target is b, consideredPeople starts with empty Set, discoveredPeople a's direct friends + a itself
  }

  println(socialConnection(testNet, "Mary", "Jim"))
  println(socialConnection(network, "Mary", "Bob"))
}

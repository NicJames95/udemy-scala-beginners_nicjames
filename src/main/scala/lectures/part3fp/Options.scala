package lectures.part3fp

import scala.util.Random

object Options extends App {

  /*
  An option is a wrapper for a value that might be present or not.
    sealed abstract class Option[+A]
    case class Some[+A](x: A) extends Option[A]
    case object None extends Option[Nothing]
      - Some wraps a concrete value
      - None is a singleton for absent values
    Options are present in many places:
      val map = Map("key" -> "value")
      map.get("key") // Some(value)
      map.get("other") // None
        ^^^ map uses options on its basic get operation; prefer it over apply
      val numbers = List(1,2,3)
      list.headOption   // Some(1)
      list.find(_ % 2 == 0)     // Some(2)
        ^^^ lots of functions on all collections work with options
  */

  val myFirstOption: Option[Int] = Some(11)
  val noOption: Option[Int] = None

  println(myFirstOption)

  // unsafe APIs
  def unsafeMethod(): String = null
 // val result = Some(unsafeMethod()) ; might get = Some(null) // WRONG
  val result = Option(unsafeMethod()) // Some or None
  println(result)

 // chained methods
  def backupMethod(): String = "A valid result"
  val chainedResult = Option(unsafeMethod()).orElse(Option(backupMethod()))

 // DESIGN unsafe APIs
  def betterUnsafeMethod(): Option[String] = None
  def betterBackupMethod(): Option[String] = Some("A valid result")

  val betterChainedResult = betterUnsafeMethod() orElse betterBackupMethod()

 // functions on Options
  println(myFirstOption.isEmpty)
  println(myFirstOption.get) // UNSAFE - DO NOT USE THIS

 // map, flatMap, filter
  println(myFirstOption.map(_ * 2))    // Some(8)
  println(myFirstOption.filter(x => x > 10))   // 4 is not greater than 10 = None
  println(myFirstOption.flatMap(x => Option(x * 10)))  // Some(40)

 // for-comprehensions

 /*
 Exercise
 */
  val config: Map[String, String] = Map(
   // fetched from elsewhere
   "host" -> "176.45.36.1",
   "port" -> "80"
 )

  class Connection {
   def connect = "Connected" // connect to some server
 }
  object Connection {
   val random = new Random(System.nanoTime())
   def apply(host: String, port: String): Option[Connection] =
     if (random.nextBoolean()) Some(new Connection)
     else None
 }

 // try to establish a connection, if so - print the connect method
  val host = config.get("host")
  val port = config.get("port")
 /* Connection logic
  if (h != null)
    if (p != null)
      return Connection.apply(h,p)

  return null
 */
  val connection = host.flatMap(h => port.flatMap(p => Connection.apply(h, p)))
 /* Connection Status logic
  if (c != null)
    return c.connect
  return null
 */
  val connectionStatus = connection.map(c => c.connect) // if connection is there then the connection status will
 // contain the "connectivity" of connection

 /* println logic
  if (connectionStatus == null) println(None) else print (Some(connectionstatus.get))
 */
  println(connectionStatus)    // if no connection prints None and will not print line below
 /* connectionStatus foreach logic
  if (status != null)
    println(status)
 */
  connectionStatus.foreach(println)

 // Alternative solution // chained calls
  config.get("host")
   .flatMap(host => config.get("port")
      .flatMap(port => Connection(host, port))
      .map(connection => connection.connect))
   .foreach(println)

 // for-comprehension
  val forConnectionStatus = for {
   host <- config.get("host")
   port <- config.get("port")
   connection <- Connection(host, port)
  } yield connection.connect
  forConnectionStatus.foreach(println)





}

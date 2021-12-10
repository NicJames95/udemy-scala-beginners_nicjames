package lectures.part2oop

import playground.{Princess => Cinderella, Prince}
import java.util.Date
import java.sql.{Date => SqlDate}

object PackagingAndImports extends App{
  // package members are accessible by their simple name
  val writer = new Writer("Daniel", "RockTheJVM", 2018)

  //import the package
  val cinderella = new Cinderella // playground.Princess = fully qualified name

  // packages are in hierarchy
  // matching folder structure

  // package object
  sayHello
  println(SPEED_OF_LIGHT)

  // imports
  val charming = new Prince

  // 1. use FQ(fully qualified) names
  val date = new Date
  val sqlDate = new SqlDate(2018, 5, 4)
  // 2. use aliasing

  // default imports
  
  // java.lang - String, Object, Exception
  // scala - Int, Nothing, Function
  // scala.Predef - println, ???

}

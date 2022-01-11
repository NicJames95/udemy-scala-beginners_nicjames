package playground

import java.io
import scala.math.Pi

object Playground
{

  class Car (milesPerGallon: Int, gallonsPerTank: Int) {
    var mpg: Int = milesPerGallon
    var gpt: Int = gallonsPerTank
    var mpt: Int = 0

    def milesPerTank(): Unit ={
      mpt = mpg * gpt
      println(s"Number of Miles Per Tank: $mpt")
    }
  }

  class Ball (diameter: Double, radius: Double, name: String) {
    var d: Double = diameter
    var r: Double = radius
    var c: Double = 0
    var n: String = name
    var a: Double = 0

    def calcCircumference(): Unit ={
      c = Pi * d
      n = name
      println(s"The circumference of the $n is: $c")
    }

    def calcDiameter(): Unit ={
      d = 2 * r
      n = name
      println(s"The diameter of the $n is: $d")
    }

    def calcArea(): Unit = {
      a = Pi * r * r
      n = name
      println(s"The area of the $n is: $a")
    }

  }




  def main(args: Array[String]): Unit =
  {
    val volvo = new Car(40, 18)
    volvo.milesPerTank()

    val baseball = new Ball(10,5, "Baseball")
    val basketball = new Ball(48,24,"Basketball")
    baseball.calcCircumference()
    basketball.calcCircumference()
    baseball.calcArea()
    basketball.calcArea()


    println("I'm ready to learn Scala!")

    println(multiplyInt(35,21))
    println(multiply(34,89))


  }

  def multiplyInt(d: Int, f: Int): Int = d * f
  val multiply = (d: Int, f: Int) => d * f


}

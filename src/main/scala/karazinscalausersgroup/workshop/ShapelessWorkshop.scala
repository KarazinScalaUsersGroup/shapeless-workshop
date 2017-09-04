package karazinscalausersgroup.workshop

import shapeless._

import scala.language.implicitConversions

object ShapelessWorkshop extends App {

  case class Red()
  case class Yellow()
  case class Green()

  type Light = Red :+: Yellow :+: Green :+: CNil

  val red: Light           = Inl(Red())
  val yellow: Light    = Inr(Inl(Yellow()))
  val green: Light = Inr(Inr(Inl(Green())))

  def printLight(light: Light): Unit = light match {
    case Inl(_)           => println("Red")
    case Inr(Inl(_))      => println("Yellow")
    case Inr(Inr(Inl(_))) => println("Green")
  }

  printLight(red)
  printLight(green)

  type StringOrDouble = String :+: Double :+: CNil

  implicit def stringToCoproduct(s: String): StringOrDouble =
    Coproduct[StringOrDouble](s)

  implicit def doubleToCoproduct(s: Double): StringOrDouble =
    Coproduct[StringOrDouble](s)

  def printStringOrDouble(value: StringOrDouble): Unit =
    println(
      value.select[String] getOrElse value.select[Double].get
    )

  printStringOrDouble(3.14)
  printStringOrDouble("Hello")

  // Error: will not compile
  // printStringOrDouble(true)

}

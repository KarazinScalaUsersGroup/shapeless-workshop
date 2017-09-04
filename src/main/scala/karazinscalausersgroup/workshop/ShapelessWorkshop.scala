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

  def print(light: Light): Unit = light match {
    case Inl(_)           => println("Red")
    case Inr(Inl(_))      => println("Yellow")
    case Inr(Inr(Inl(_))) => println("Green")
  }

  print(red)
  print(green)

  type StringOrDouble = String :+: Double :+: CNil

  implicit def stringToCoproduct(s: String): StringOrDouble =
    Coproduct[StringOrDouble](s)

  implicit def doubleToCoproduct(s: Double): StringOrDouble =
    Coproduct[StringOrDouble](s)

  def print(value: StringOrDouble): Unit =
    println(
      value.select[String] getOrElse value.select[Double].get
    )


  print(3.14)
  print("Hello")

  // Error: will not compile
  // process(4)

}

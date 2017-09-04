package karazinscalausersgroup.workshop

import shapeless._

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

}

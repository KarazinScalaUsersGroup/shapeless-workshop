package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  sealed trait Shape
  case class Rectangle(width: Double, height: Double) extends Shape
  case class Circle(radius: Double) extends Shape

  val genShape = Generic[Shape]

  println(genShape.to(Rectangle(3.0, 4.0)))
  println(genShape.from(Inr(Inl(Rectangle(3.0, 4.0)))))

  println(genShape.to(Rectangle(3.0, 4.0)))
  println(genShape.from(Coproduct[genShape.Repr](Rectangle(3.0, 4.0))))

}

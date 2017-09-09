package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  trait Encoder[A] {
    def encode(value: A): List[String]
  }

  case class Employee(name: String, number: Int, manager: Boolean)

  implicit val employeeEncoder: Encoder[Employee] =
    new Encoder[Employee] {
      def encode(value: Employee): List[String] =
        value.name :: value.number.toString :: value.manager.toString :: Nil
    }

  implicit def pairEncoder[A, B](implicit
                                 aEncoder: Encoder[A],
                                 bEncoder: Encoder[B]): Encoder[(A, B)] =
    new Encoder[(A, B)] {
      def encode(value: (A, B)): List[String] =
        aEncoder.encode(value._1) ::: bEncoder.encode(value._2)
    }

  implicit def listEncoder[T](implicit encoder: Encoder[T]): Encoder[List[T]] =
    new Encoder[List[T]] {
      def encode(value: List[T]): List[String] =
        value flatMap  { v => encoder.encode(v) }
    }

  def encode[T](value: T)(implicit encoder: Encoder[T]) =
    encoder.encode(value)

  val alice = Employee("Alice", 42, manager = true)
  val bob = Employee("Bob", 19, manager = false)
  val dave = Employee("Dave", 17, manager = false)
  val anna = Employee("Anna", 43, manager = false)

  val list = List((alice, bob), (dave, anna))

  println(encode(list))

}

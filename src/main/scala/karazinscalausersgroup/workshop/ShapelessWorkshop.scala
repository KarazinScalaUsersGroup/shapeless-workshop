package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  trait Op[Left, Right] {
    type Result

    def add(left: Left, right: Right): Result
    def multiply(left: Left, right: Right): Result
  }

  implicit val intIntOp: Int Op Int = new Op[Int, Int] {
    type Result = Int

    def add(left: Int, right: Int): Int = left + right
    def multiply(left: Int, right: Int): Int = left * right
  }

  implicit val intStringOp: Int Op String = new Op[Int, String] {
    type Result = String

    def add(left: Int, right: String): String = left + right
    def multiply(left: Int, right: String): String = right * left
  }

  object Op {
    def apply[Left, Right](implicit op: Op[Left, Right]): Op[Left, Right] = op

    def add[Left, Right](left: Left, right: Right)(implicit op: Op[Left, Right]) =
      op.add(left, right)

    def multiply[Left, Right](left: Left, right: Right)(implicit op: Op[Left, Right]) =
      op.multiply(left, right)
  }

  println(Op[Int, String].add(42, "hello"))
  println(Op[Int, String].multiply(2, "hello"))

  println(Op.add(42, 24))
  println(Op.multiply(42, 24))

}

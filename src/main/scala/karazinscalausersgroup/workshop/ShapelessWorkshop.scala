package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  case class Employee(name: String, number: Int, manager: Boolean)

  trait First[L <: HList] {
    type Out
    def apply(hList: L): Out
  }

  object First {
    def apply[L <: HList](hList: L)(implicit first: First[L]) =
      first.apply(hList)
  }

  // We don't need to implement implicit First for HNil

  implicit def firstHList[H, T <: HList]: First[H :: T] =
    new First[H :: T] {
      type Out = H
      def apply(hList: H :: T): H = hList.head
    }

  trait Second[L <: HList] {
    type Out
    def apply(hList: L): Out
  }

  object Second {
    def apply[L <: HList](hList: L)(implicit second: Second[L]) =
      second.apply(hList)
  }

  // We don't need to implement implicit Second for HNil

  implicit def secondHList[F, S, T <: HList]: Second[F :: S :: T] =
    new Second[F :: S :: T] {
      type Out = S
      def apply(hList: F :: S :: T): S = hList.tail.head
    }

  trait Last[L <: HList] {
    type Out
    def apply(hList: L): Out
  }

  object Last {
    type Aux[L <: HList, Out0] = Last[L] { type Out = Out0 }

    def apply[L <: HList](hList: L)(implicit last: Last[L]): last.Out =
      last.apply(hList)
  }

  // We don't need to implement implicit Last for HNil

  implicit def lastSingleHList[H]: Last.Aux[H :: HNil, H] =
    new Last[H :: HNil] {
      type Out = H
      def apply(hList: H :: HNil): Out = hList.head
    }

  implicit def lastHList[H, T <: HList](implicit last: Last[T]): Last.Aux[H :: T, last.Out] =
    new Last[H :: T] {
      type Out = last.Out
      def apply(hList: H :: T) = last.apply(hList.tail)
    }

  def lastField[T, L <: HList](value: T)(implicit
                                         generic: Generic.Aux[T, L],
                                         last: Last[L]
  ): last.Out = last(generic.to(value))


  println(First(42 :: "Hello" :: HNil))

  println(Second(42 :: "Hello" :: HNil))

  val last: Boolean = Last("Hello" :: 42 :: true :: HNil)
  println(Last("Hello" :: 42 :: true :: HNil))

  println(lastField(Employee("John", 42, manager = true)))

}

package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  trait Encoder[A] {
    def encode(value: A): List[String]
  }

  object Encoder {
    def apply[T](func: T => List[String]) =
      new Encoder[T] {
        def encode(value: T) = func(value)
      }
  }

  implicit def pairEncoder[A, B](implicit
                                 aEncoder: Encoder[A],
                                 bEncoder: Encoder[B]): Encoder[(A, B)] =
    Encoder[(A, B)]( value =>
      aEncoder.encode(value._1) ::: bEncoder.encode(value._2)
    )

  implicit def listEncoder[T](implicit encoder: Encoder[T]): Encoder[List[T]] =
    Encoder[List[T]]( value =>
      value flatMap  { v => encoder.encode(v) }
    )

  implicit val encodeHNil: Encoder[HNil] =
    Encoder[HNil](_ => Nil)

  implicit def encodeHLis[H, T <: HList](implicit
                                         hEncoder: Encoder[H],
                                         tEncoder: Encoder[T]): Encoder[H :: T] =
    Encoder[H :: T]( value =>
      hEncoder.encode(value.head) ::: tEncoder.encode(value.tail)
    )

  implicit val encodeString: Encoder[String] =
    Encoder[String](value => value :: Nil)

  implicit val encodeInt: Encoder[Int] =
    Encoder[Int](value => value.toString :: Nil)

  implicit val encodeBoolean: Encoder[Boolean] =
    Encoder[Boolean](value => value.toString :: Nil)

  def encode[T](value: T)(implicit encoder: Encoder[T]) =
    encoder.encode(value)

  println(encode(List("Hello" :: 42 :: true :: HNil) :: List((42, true)) :: HNil))

}

package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  trait Encoder[A] {
    def encode(value: A): List[String]
  }

  implicit val intEncoder: Encoder[Int] =
    new Encoder[Int] {
      def encode(value: Int) = value.toString :: Nil
    }

  implicit val doubleEncoder: Encoder[Double] =
    new Encoder[Double] {
      def encode(value: Double) = value.toString :: Nil
    }

  implicit val stringEncoder: Encoder[String] =
    new Encoder[String] {
      def encode(value: String) = value :: Nil
    }

  implicit val hnilEncoder: Encoder[HNil] =
    new Encoder[HNil] {
      def encode(value: HNil) = Nil
    }

  implicit def hlistEncoder[H, T <: HList]( implicit
                                            // Wrap head in Lazy
                                            hEncoder: Lazy[Encoder[H]],
                                            tEncoder: Encoder[T]
                                          ): Encoder[H :: T] =
    new Encoder[H :: T] {
      def encode(value: H :: T) = value match {
        // Lazy head
        case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
      }
    }

  implicit val cnilEncoder: Encoder[CNil] =
    new Encoder[CNil] {
      def encode(value: CNil) =
      // Dead code. We will never get it because
      // we can't build CNil!
        throw new Exception("Unattainable!")
    }

  implicit def coproductEncoder[H, T <: Coproduct](implicit
                                                   // Wrap head in Lazy                                                  hEncoder: Encoder[H],
                                                   hEncoder: Lazy[Encoder[H]],
                                                   tEncoder: Encoder[T]): Encoder[H :+: T] =
    new Encoder[H :+: T] {
      def encode(value: H :+: T) = value match {
        // Lazy head
        case Inl(h) => hEncoder.value.encode(h)
        case Inr(t) => tEncoder.encode(t)
      }
    }

  implicit def genericEncoder[A, R](implicit
                                    gen: Generic.Aux[A, R],
                                    // Wrap head in Lazy
                                    rEncoder: Lazy[Encoder[R]]
                                   ): Encoder[A] =
    new Encoder[A] {
      def encode(value: A) =
      // Lazy representation
        rEncoder.value.encode(gen.to(value))
    }

  // Bar has richer constructor than `Foo`.
  // Compiler can't deal such things
  // without Lazy trick
  case class Bar(baz: Int, qux: String)
  case class Foo(bar: Bar)

  // Works only with Lazy
  implicitly[Encoder[Foo]]

}

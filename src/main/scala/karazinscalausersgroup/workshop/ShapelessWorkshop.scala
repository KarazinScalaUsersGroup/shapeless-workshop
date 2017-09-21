package karazinscalausersgroup.workshop

import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist._
import shapeless.syntax.singleton._

object ShapelessWorkshop extends App {

  sealed trait JsonValue
  case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
  case class JsonArray(items: List[JsonValue]) extends JsonValue
  case class JsonString(value: String) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  trait JsonEncoder[A] {
    def encode(value: A): JsonValue
  }

  object JsonEncoder {
    def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

    def create[A](func: A => JsonValue): JsonEncoder[A] =
      new JsonEncoder[A] {
        def encode(value: A): JsonValue = func(value)
      }
  }

  implicit val booleanEncoder: JsonEncoder[Boolean] =
    JsonEncoder.create(bool => JsonBoolean(bool))

  implicit val intEncoder: JsonEncoder[Int] =
    JsonEncoder.create(num => JsonNumber(num))

  implicit val doubleEncoder: JsonEncoder[Double] =
    JsonEncoder.create(num => JsonNumber(num))

  implicit val stringEncoder: JsonEncoder[String] =
    JsonEncoder.create(str => JsonString(str))

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    JsonEncoder.create(list => JsonArray(list.map(enc.encode)))

  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    JsonEncoder.create(opt => opt.map(enc.encode).getOrElse(JsonNull))

  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(value: A): JsonObject
  }

  object JsonObjectEncoder {

    def create[A](fn: A => JsonObject): JsonObjectEncoder[A] =
      new JsonObjectEncoder[A] {
        def encode(value: A): JsonObject =
          fn(value)
      }
  }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    JsonObjectEncoder.create(hnil => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
                                                              witness: Witness.Aux[K],
                                                              hEncoder: Lazy[JsonEncoder[H]],
                                                              tEncoder: JsonObjectEncoder[T]
                                                             ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    JsonObjectEncoder.create { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  implicit def genericObjectEncoder[A, H <: HList](implicit
                                                   generic: LabelledGeneric.Aux[A, H],
                                                   hEncoder: Lazy[JsonObjectEncoder[H]]
                                                  ): JsonEncoder[A] =
    JsonObjectEncoder.create { value =>
      hEncoder.value.encode(generic.to(value))
    }

  case class Document(path: String, size: Int, readOnly: Boolean)

  val document = Document("/", 42, readOnly = true)

  println(JsonEncoder[Document].encode(document))
}

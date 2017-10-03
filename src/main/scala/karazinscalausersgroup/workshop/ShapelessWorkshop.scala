package karazinscalausersgroup.workshop

import shapeless._
import shapeless.ops.hlist
import cats.Monoid
import shapeless.labelled.{FieldType, field}

object ShapelessWorkshop extends App {

  // Repr is String :: HNil
  case class DocumentName(name: String)

  // Repr is String :: Int :: HNil
  case class Document(name: String, size: Int)

  // Repr is Int :: String :: HNil
  case class AnotherDocument(size: Int, name: String)

  // Repr is String :: Int :: String :: HNil
  case class PersistedDocument(name: String, size: Int, createdBy: String)

  // Repr is Int :: Boolean :: Option[Int]
  case class StrangeDocument(size: Int, isReady: Boolean, max: Option[Int])

  trait Migration[A, B] {
    def apply(a: A): B
  }

  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration.apply(a)
  }

  // We use Monoid only for defaults values
  // In fact we don't need to implement (A, A) => A,
  // but lets leave a clean code
  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
  new Monoid[A] {
    def empty = zero

    def combine(l: A, r: A) = add(l, r)
  }

  // There is no Monoid for Boolean in cats out of the box
  implicit val boolMonoid: Monoid[Boolean] =
    createMonoid[Boolean](false)((l, r) => l && r)

  implicit val hnilMonoid: Monoid[HNil] =
    createMonoid[HNil](HNil)((l, r) => HNil)

  implicit def hList[K <: Symbol, H, T <: HList]
  (implicit
   hMonoid: Lazy[Monoid[H]],
   tMonoid: Monoid[T]
  ): Monoid[FieldType[K, H] :: T] =
    createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) {
      (l, r) =>
        field[K](hMonoid.value.combine(l.head, r.head)) ::
          tMonoid.combine(l.tail, r.tail)
    }


  implicit def genericMigration[
    Source, // In our example it's the Document
    Target, // In our example it's DocumentName or AnotherDocument or PersistedDocument or StrangeDocument
    SourceRepr <: HList,
    TargetDocRepr <: HList,
    Common <: HList, // Common fields in Source and Target (in fact it's hlist repr intersection)
    Added <: HList, // New fields to transform Source to Target
    Unaligned <: HList // Unsorted result of adding new fields to the Common repr
  ](implicit
    sourceRepr: LabelledGeneric.Aux[Source, SourceRepr],
    targetRepr: LabelledGeneric.Aux[Target, TargetDocRepr],
    inter: hlist.Intersection.Aux[SourceRepr, TargetDocRepr, Common],
    dif: hlist.Diff.Aux[TargetDocRepr, Common, Added],
    monoid: Monoid[Added],
    prepend: hlist.Prepend.Aux[Added, Common, Unaligned],
    align: hlist.Align[Unaligned, TargetDocRepr]
   ): Migration[Source, Target] =
    new Migration[Source, Target] {
      def apply(a: Source) =
        targetRepr.from(          // Create Target from Repr
          align(                  // Sort Added + Common by Target Repr pattern
            prepend(              // Combine added + common
              monoid.empty,       // Fill added fields with defaults values
              inter(              // Intersect Source Repr with Target Repr
                sourceRepr.to(a)  // Source Repr
              )
            )
          )
        )
    }

  println(Document("Hello", 42).migrateTo[DocumentName])
  println(Document("Hello", 42).migrateTo[AnotherDocument])
  println(Document("Hello", 42).migrateTo[PersistedDocument])
  println(Document("Hello", 42).migrateTo[StrangeDocument])

}

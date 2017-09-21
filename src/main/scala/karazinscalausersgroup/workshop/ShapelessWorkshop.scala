package karazinscalausersgroup.workshop

import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist._
import shapeless.syntax.singleton._

object ShapelessWorkshop extends App {

  case class Document(name: String, size: Int)

  case class PersitedDocument(name: String,
                              id: Long,
                              size: Int,
                              createdBy: String)

  class Build[Targer /*PersitedDocument*/ ] {
    def from[
              Source,                       // Document
              TargetRepr <: HList,          // PersitedDocument HList
              SourceRepr <: HList,          // Document HList
              Fields <: HList,              // fields
              SourceWithFiledsRepr <: HList // field + Document HList
    ](source: Source, fields: Fields)
     (implicit
      // PersistedDocument representation String :: Int :: String :: HNil
      targetGen: LabelledGeneric.Aux[Targer, TargetRepr],

      // Document representation String :: Int :: HNil
      sourceGen: LabelledGeneric.Aux[Source, SourceRepr],

      // Document with added new field String :: String :: Int :: HNil
      // Take into account that we can add fields only as a head of HList
      sourceWithFieldsRepr: Prepend.Aux[Fields, SourceRepr, SourceWithFiledsRepr],

      // Reordered representation of Document with add field String :: Int :: String:: HNil
      // by PersistedDocument representation pattern
      align: Align[SourceWithFiledsRepr, TargetRepr]
     ) = {

      targetGen.from(          // <- HList into PersistedDocument
        sourceWithFieldsRepr(  // <- Add new Fields to the Document representation
          fields,
          sourceGen.to(source) // <- Document into HList
        ).align[TargetRepr]    // <- Reorder result as in PersistedDocument
      )
    }
  }

  object Build {
    def apply[A] = new Build[A]
  }

  val result =
    Build[PersitedDocument].from(Document("hello", 42), 'id ->> 17L :: 'createdBy ->> "user" :: HNil)

  println(result)

}

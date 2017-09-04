package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  case class Document(path: String, size: Long, readOnly: Boolean)

  val documentGen = Generic[Document]
  val document = Document("xyz", 42L, readOnly = true)

  val documentRepr = documentGen.to(document)
  val doc1 = documentGen.from(documentGen.to(document))
  val doc2 = documentGen.from("abc" :: 42L :: true :: HNil)

  case class PersistedDocument(createdBy: String, path: String, size: Long, readOnly: Boolean)
  val persistedDocumentRep = Generic[PersistedDocument]

  val persistedDocument = persistedDocumentRep.from("user@company.com" :: documentRepr)
  println(persistedDocument)

  val tupleGen = Generic[(String, Int, Boolean)]
  println(tupleGen.to(("Hello", 42, true)))
  println(tupleGen.from("Hello" :: 42 :: true :: HNil))

}

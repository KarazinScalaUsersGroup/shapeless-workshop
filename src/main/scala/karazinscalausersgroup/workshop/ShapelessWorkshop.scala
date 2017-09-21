package karazinscalausersgroup.workshop

import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist._
import shapeless.syntax.singleton._

object ShapelessWorkshop extends App {

  case class Document(path: String, size: Int, readOnly: Boolean)

  val document = Document("/", 42, readOnly = true)

  val docGen = LabelledGeneric[Document]

  println(docGen.to(document))
  println(docGen.from('path ->> "/" :: 'size ->> 42 :: 'readOnly ->> true :: HNil))
}

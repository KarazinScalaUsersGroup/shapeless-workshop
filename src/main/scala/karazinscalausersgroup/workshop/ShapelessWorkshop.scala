package karazinscalausersgroup.workshop

import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist._
import shapeless.syntax.singleton._

object ShapelessWorkshop extends App {

  val x: Witness.`42`.T = 42.narrow

  // `42` behaves as regular Int
  val y: Int = x + 1

  trait HttpPort

  def initPortWithPhantomType(port: Int with HttpPort) = {}

  val number = 80
  val portWithPhantomType = number.asInstanceOf[Int with HttpPort]

  initPortWithPhantomType(portWithPhantomType)
  // Pure Int can't be provided
  // initPortWithPhantomType(80)


  val portWithTag = 'port ->> 80

  def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K =
    witness.value

  def getFieldValue[K, V](value: FieldType[K, V]): V =
    value

  def initPortWithTag[K, V](port: FieldType[K, V]) = {}

  initPortWithTag(portWithTag)
  // Pure Int can't be provided
  // initPortWithTag(80)

  println(getFieldName(portWithTag))
  println(getFieldValue(portWithTag))

}

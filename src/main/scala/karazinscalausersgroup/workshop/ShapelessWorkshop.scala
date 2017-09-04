package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  val product: String :: Int :: Boolean :: HNil =
    "hello" :: 42 :: true :: HNil

  val first: String = product.head
  val second: Int = product.tail.head
  val rest: Boolean :: HNil = product.tail.tail

  //   Error
  //   val error = product.tail.tail.tail.head

  val newProduct: Long :: String :: Int :: Boolean :: HNil =
    42L :: product

}

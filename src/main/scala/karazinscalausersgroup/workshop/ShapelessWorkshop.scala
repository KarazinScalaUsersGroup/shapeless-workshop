package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  case class Document(name: String, size: Int)

  // Add function over Ints and Strings
  object add extends Poly2 {

    implicit val intCase: Case.Aux[Int, Int, Int] =
      at((left, right) => left + right)

    implicit val stringCase: Case.Aux[String, String, String] =
      at((left, right) => left + right)
  }

  assert(add(17, 19) == 36)
  assert(add("hello", " world") == "hello world")


  object poly0 extends Poly0 {
    implicit val intCase: Case0[Int] =
      at[Int](42)
  }

  // `Size of` for Ints, Strings and Tuples and Lists over Ints and Strings
  object sizeOf extends Poly1 {

    implicit val intCase: Case.Aux[Int, Int] =
      at(int => int)

    implicit val stringCase: Case.Aux[String, Int] =
      at(str => str.length)

    implicit def tupleCase[L, R](implicit
                                 leftCase: Case.Aux[L, Int],
                                 rightCase: Case.Aux[R, Int]
                                ) =
      at[(L, R)](tuple => sizeOf(tuple._1) + sizeOf(tuple._2))

    implicit def listCase[T](implicit listC: Case.Aux[T, Int]) =
      at[List[T]](list => list.foldLeft(0)((acc, value) => acc + sizeOf(value)))

  }

  assert(sizeOf(42) == 42)
  assert(sizeOf("Hello world") == 11)
  assert(sizeOf((42, "Hello world")) == 53)
  assert(sizeOf(List(1, 2, 3)) == 6)


  val docRepr = Generic[Document].to(Document("War and Peace", 42))

  // `Size of` for Ints, Strings for compound objects
  object sizeOf2 extends Poly2 {
    implicit val intCase: Case.Aux[Int, Int, Int] =
      at((acc, value) => acc + value)

    implicit val stringCase: Case.Aux[Int, String, Int] =
      at((acc, value) => acc + value.length)
  }

  assert(docRepr.map(sizeOf) == 13 :: 42 :: HNil)
  assert(docRepr.foldLeft(0)(sizeOf2) == 55)

}

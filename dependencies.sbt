

libraryDependencies ++= {
  val shapelessV = "2.3.2"
  val catsV      = "1.0.0-MF"

  Seq(
    "com.chuusai"       %% "shapeless"             % shapelessV,
    "org.typelevel"     %% "cats-core"             % catsV
  )
}
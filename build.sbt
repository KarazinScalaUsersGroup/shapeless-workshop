organization := "karazinscalausersgroup"

name := "shapeless-workshop"

version := "1.0"

scalaVersion := "2.12.3"

resolvers ++= Seq(
  "Maven Central Server"          at "http://repo1.maven.org/maven2",
  "TypeSafe Repository Releases"  at "http://repo.typesafe.com/typesafe/releases/",
  "TypeSafe Repository Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
  "Sonatype"                      at "https://oss.sonatype.org/content/groups/public"
)

resolvers += Resolver.mavenLocal


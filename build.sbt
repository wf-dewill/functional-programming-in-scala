name := "functional-programming-scala"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
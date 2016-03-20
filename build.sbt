name := "OEIS"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.mongodb" %% "casbah" % "3.1.1"

def readme(base : File) = base / "README"
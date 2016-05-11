name := "OEIS"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.mongodb" %% "casbah" % "2.8.2"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.3.0"

libraryDependencies +=   "com.novus" %% "salat" % "1.9.9"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.play" %% "play-ws" % "2.4.3"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.3.0"

def readme(base : File) = base / "README"
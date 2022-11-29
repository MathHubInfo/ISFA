
name := "ISFA_puzzle"

version := "0.2"

scalaVersion := "2.12.12"

val http4sVersion = "0.23.16"

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.0.0-M1"

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-parser-combinators
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

// https://mvnrepository.com/artifact/org.mongodb/casbah
libraryDependencies += "org.mongodb" %% "casbah" % "3.1.1"

// https://mvnrepository.com/artifact/org.scalaj/scalaj-http
libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.2"

// https://mvnrepository.com/artifact/com.github.salat/salat
libraryDependencies += "com.github.salat" %% "salat" % "1.11.2"

//TODO
//resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Typesafe Repo" at "https://dl.bintray.com/typesafe/maven-releases/"

// https://mvnrepository.com/artifact/com.typesafe.play/play-ws
libraryDependencies += "com.typesafe.play" %% "play-ws" % "2.8.7"

// https://mvnrepository.com/artifact/org.json4s/json4s-native
libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.10"


// GeneratingFunctionsSagePackage.scalae was missing the SLF4J lib? now it runs
libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "io.circe" %% "circe-generic" % "0.14.3",
  "io.circe" %% "circe-literal" % "0.14.3",
  "org.typelevel" %% "jawn-parser" % "1.3.2",
  "org.typelevel" %% "jawn-ast" % "1.3.2"
)
excludeDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl"

// scalacOptions ++= Seq("-Ypartial-unification")

/*'''

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
//had to change scala version for this on:
libraryDependencies += "org.mongodb" %% "casbah" % "3.1.1"
//this one worked instantly:
libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.3.0"
//scala version for this one is even lower
libraryDependencies +=   "com.novus" %% "salat" % "1.9.9"
//resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Typesafe Repo" at "https://dl.bintray.com/typesafe/maven-releases/"
libraryDependencies += "com.typesafe.play" %% "play-ws" % "2.4.3"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.3.0"
//libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.10"
'''*/

def readme(base : File) = base / "README"


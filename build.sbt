name := "Skene"

organization := "com.roundeights"

version := "0.1"

scalaVersion := "2.10.2"

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-feature")

// Application dependencies
libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-actors" % "2.10.2",
    "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided",
    "org.slf4j" % "slf4j-simple" % "1.7.2",
    "org.specs2" %% "specs2" % "1.13" % "test",
    "org.mockito" % "mockito-all" % "1.9.5" % "test"
)


name := "Skene"

organization := "com.roundeights"

version := "0.2.0"

scalaVersion := "2.10.3"

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-feature")

publishTo := Some("Spikemark" at "https://spikemark.herokuapp.com/maven/roundeights")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

// Application dependencies
libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-actors" % "2.10.3",
    "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided",
    "org.slf4j" % "slf4j-simple" % "1.7.5",
    "org.specs2" %% "specs2" % "2.3.4" % "test"
)


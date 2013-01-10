name := "Skene"

scalaVersion := "2.10.0-RC5"

version := "0.1"

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-feature")

// Repositories in which to find dependencies
resolvers ++= Seq(
    "Specs Repository" at "http://oss.sonatype.org/content/repositories/releases"
)

// Application dependencies
libraryDependencies ++= Seq(
    "org.eclipse.jetty" % "jetty-webapp" % "8.1.7.v20120910" % "test,compile",
    "org.eclipse.jetty" % "jetty-server" % "8.1.7.v20120910" % "test,compile",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" artifacts (
        Artifact("javax.servlet", "jar", "jar")
    ),
    "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided->default",
    "org.slf4j" % "slf4j-simple" % "1.7.2",
    "org.specs2" %% "specs2" % "1.12.3" % "test",
    "org.mockito" % "mockito-all" % "1.9.5-rc1" % "test"
)


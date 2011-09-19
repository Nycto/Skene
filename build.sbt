name := "Skene"

scalaVersion := "2.9.1"

// Add support for the sbt web plugin
seq(webSettings :_*)

// append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

// Repositories in which to find dependencies
resolvers ++= Seq(
    "Maven Repository" at "http://repo1.maven.org/maven2/org/",
    "Specs Repository" at "http://scala-tools.org/repo-releases"
)

// Application dependencies
libraryDependencies ++= Seq(
    "org.eclipse.jetty" % "jetty-webapp" % "8.0.1.v20110908" % "jetty,test,compile",
    "org.eclipse.jetty" % "jetty-server" % "8.0.1.v20110908" % "jetty,test,compile",
    "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided->default",
    "junit" % "junit" % "4.9" % "test",
    "org.specs2" %% "specs2" % "1.6.1" % "test",
    "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test",
    "org.mockito" % "mockito-all" % "1.8.5" % "test"
)

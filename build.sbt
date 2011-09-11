name := "Skene"

scalaVersion := "2.9.1"

resolvers ++= Seq(
    "Maven Repository" at "http://repo1.maven.org/maven2/org/",
    "Specs Repository" at "http://scala-tools.org/repo-releases"
)

libraryDependencies ++= Seq(
    "org.eclipse.jetty" % "jetty-webapp" % "8.0.1.v20110908",
    "org.eclipse.jetty" % "jetty-server" % "8.0.1.v20110908",
    "javax.servlet" % "javax.servlet-api" % "3.0.1",
    "junit" % "junit" % "4.9" % "test",
    "org.specs2" %% "specs2" % "1.6.1" % "test",
    "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test"
)

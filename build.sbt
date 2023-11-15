val akkaVersion = "2.7.0"
val AkkaHttpVersion = "10.5.0-M1"
val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "ch.usi.si.msde.edsl",
    name := "assignment-03-template",
    version := "2023.1",
    scalacOptions += "-feature",
    scalacOptions += "-language:implicitConversions",
    scalaVersion := scala3Version,
    fork := true,
    libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion,
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
    libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % AkkaHttpVersion
  )

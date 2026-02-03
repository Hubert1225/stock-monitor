val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Stock Monitor",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.4" % Test,
      "org.scala-lang" %% "toolkit" % "0.2.0"
    )
  )

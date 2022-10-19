ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "DSL",
    libraryDependencies ++= Seq(
      "com.github.pureconfig" %% "pureconfig" % "0.17.1",
      "org.typelevel" %% "cats-effect" % "3.3.14",
      "com.chuusai" %% "shapeless" % "2.3.10",
      compilerPlugin(("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full))
    )
  )

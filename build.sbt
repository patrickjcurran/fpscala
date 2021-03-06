import sbt.CrossVersion

val commonSettings = Seq(
  scalaVersion := "2.13.4"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.2" cross CrossVersion.full)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  ).settings(
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.2" cross CrossVersion.full)
)
lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )


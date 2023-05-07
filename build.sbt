val scalaVersion_3 = "3.2.2"

lazy val commonSettings = Seq(
    organization := "com.github.tototoshi",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scalaVersion_3,
    scalacOptions ++= Seq("-Ykind-projector:underscores"),
    crossScalaVersions := Seq(scalaVersion_3),
)

lazy val module = project
  .in(file("module"))
  .settings(
    commonSettings,
    name := "freearg",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "2.9.0",
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.4.10" % Test,
      "org.scalatest" %% "scalatest" % "3.2.15" % "test"
    )
  )

lazy val example = project
  .in(file("example"))
  .settings(
    commonSettings,
    name := "freearg-example",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.4.10",
    )
  )
  .dependsOn(module)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .aggregate(module, example)

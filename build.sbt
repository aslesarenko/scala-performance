val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "perf-guide",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "spire" % "0.18.0",
      ("com.storm-enroute" %% "scalameter" % "0.21" % Test).cross(CrossVersion.for3Use2_13),
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )

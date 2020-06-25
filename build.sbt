lazy val core = project.in(file("modules/core"))
  .settings(
    scalaVersion := "2.12.11",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.1.1",
      "org.typelevel" %% "cats-effect" % "2.1.3",
      "com.lihaoyi" %% "sourcecode" % "0.1.9",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val example = project.in(file("modules/example"))
  .dependsOn(core)
  .settings(
    scalaVersion := "2.12.11",
    //scalacOptions ++= Seq("-Ymacro-debug-lite", "-Yshow-trees-compact")
  )

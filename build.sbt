val commonSettings = Seq(
  scalaVersion := "2.12.12",
  //scalaVersion := "0.26.0-RC1",
  crossScalaVersions := Seq("2.12.12", "0.26.0-RC1")
)

lazy val core = project.in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies += "org.scalameta" %% "munit" % "0.4.3" % "test",
    testFrameworks += new TestFramework("munit.Framework"),
    libraryDependencies ++= Seq(
      ("org.typelevel" %% "cats-core" % "2.1.1").withDottyCompat(scalaVersion.value),
      ("org.typelevel" %% "cats-effect" % "2.1.3").withDottyCompat(scalaVersion.value),
      "com.lihaoyi" %% "sourcecode" % "0.1.9",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test",
      "org.scala-lang" % "scala-library" % scalaVersion.value % "test",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai" %% "shapeless" % "2.3.3"
    ),
    fork in Test := true,
    fork in testOnly := true,
    scalacOptions in run := Seq.empty
  )

lazy val core2 = project.in(file("modules/core2"))
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.12",
    scalacOptions ++= Seq(
      "-language:experimental.macros"
    ),
    sourceGenerators in Compile += (sourceManaged in Compile).map(dir => Boilerplate.gen(dir)).taskValue,
    libraryDependencies += "org.scalameta" %% "munit" % "0.4.3" % "test",
    testFrameworks += new TestFramework("munit.Framework"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.1.1",
      "org.typelevel" %% "cats-effect" % "2.1.3",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val example = project.in(file("modules/example"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    //scalacOptions ++= Seq("-Ymacro-debug-lite", "-Yshow-trees-compact")
  )

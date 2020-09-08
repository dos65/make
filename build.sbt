val commonSettings = Seq(
  scalaVersion := "2.13.3",
  organization := "io.github.dos65",
  crossScalaVersions := Seq("2.12.12", "2.13.3")
)

lazy val make = project.in(file("modules/make"))
  .settings(commonSettings)
  .settings(
    name := "make",
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
  .dependsOn(make)
  .settings(commonSettings)

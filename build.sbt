val commonSettings = Seq(
  scalaVersion := "2.13.3",
  organization := "io.github.dos65",
  version := "0.0.1",
  crossScalaVersions := Seq("2.12.12", "2.13.3"),
  libraryDependencies ++= {
    if (is213(scalaVersion.value))
      Seq.empty
    else
      Seq(compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.patch)))
  },
  scalacOptions ++= {
    if(is213(scalaVersion.value)) 
      Seq("-Ymacro-annotations")
    else
      Nil
  },
  libraryDependencies += "org.scalameta" %% "munit" % "0.4.3" % "test",
  testFrameworks += new TestFramework("munit.Framework"),
)

lazy val core = project.in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    name := "make-core",
    scalacOptions ++= Seq(
      "-language:experimental.macros"
    ),
    sourceGenerators in Compile += (sourceManaged in Compile).map(dir => Boilerplate.gen(dir)).taskValue,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-effect" % "2.1.3" % "test"
    ),
  )

lazy val makeCatsEffect = project.in(file("modules/cats-effect"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "make-cats-effect",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "2.1.3"
    ),
  )

lazy val makeZio = project.in(file("modules/zio"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "make-zio",
    scalacOptions ++= Seq(
      "-language:experimental.macros"
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "0.4.3" % "test",
    testFrameworks += new TestFramework("munit.Framework"),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "1.0.1"
    )
  )

lazy val example = project.in(file("modules/example"))
  .dependsOn(makeCatsEffect, makeZio)
  .settings(commonSettings)
  .settings(
    name := "make-example",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
  )

def is213(v: String): Boolean = {
  CrossVersion.partialVersion(v) match {
    case Some((2, 13)) => true 
    case _ => false
  }
}

import xerial.sbt.Sonatype._

lazy val commonSettings = Seq(
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

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := sonatypePublishToBundle.value,
  sonatypeProfileName := "io.github.dos65",
  sonatypeProjectHosting := Some(GitHubHosting("dos65", "make", "qtankle@gmail.com")),
  licenses := Seq("Apache 2.0 License" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  sonatypeBundleDirectory := (ThisBuild / baseDirectory).value / "target" / "sonatype-staging" / s"${version.value}"
)

lazy val make = project.in(file("modules/core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "make",
    scalacOptions ++= Seq(
      "-language:experimental.macros"
    ),
    sourceGenerators in Compile += (sourceManaged in Compile).map(dir => Boilerplate.gen(dir)).taskValue,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.1.1",
      "org.typelevel" %% "cats-effect" % "2.1.3" % "test"
    ),
  )

lazy val example = project.in(file("modules/example"))
  .dependsOn(make)
  .settings(commonSettings)
  .settings(
    name := "make-example",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "2.1.3",
      "dev.zio" %% "zio" % "1.0.1",
      "dev.zio" %% "zio-interop-cats" % "2.1.4.0",
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
    skip in publish := true
  )

lazy val rootProject = project.in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "make-root",
    skip in publish := true
  )
  .aggregate(make)

def is213(v: String): Boolean = {
  CrossVersion.partialVersion(v) match {
    case Some((2, 13)) => true 
    case _ => false
  }
}

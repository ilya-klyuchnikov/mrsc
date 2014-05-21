import sbt._
import Keys._
import bintray.Plugin._

object MRSCBuild extends Build {

  override lazy val settings = super.settings ++ Seq(scalaVersion := "2.11.0")

  lazy val MRSCProject = Project("mrsc", file("src/mrsc"),
    settings = Project.defaultSettings ++ Seq(
      organization := "mrsc",
      name := "mrsc",
      version := "0.5.1",
      libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6",
      libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test",
      unmanagedBase := file("lib"),
      //fork := true,
      baseDirectory in run := file("."),
      testOptions in Test += Tests.Argument("-oD"),
      licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
    ) ++ bintrayPublishSettings
  )

  lazy val SamplesProject = Project("samples", file("src/samples"),
    settings = Project.defaultSettings ++ Seq(
      libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6",
      //fork := true,
      baseDirectory in run := file(".")
    )
  ) dependsOn(MRSCProject)

  lazy val ArraysProject = Project("arrays", file("src/arrays"),
    settings = Project.defaultSettings ++ Seq(
      libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6",
      //fork := true,
      baseDirectory in run := file(".")
    )
  ) dependsOn(MRSCProject)

  lazy val root = Project(id = "parent", base = file(".")) aggregate(MRSCProject, SamplesProject, ArraysProject)
}

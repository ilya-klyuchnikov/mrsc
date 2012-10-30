import sbt._
import Keys._

object MRSCBuild extends Build {

  override lazy val settings = super.settings ++ Seq(scalaVersion := "2.9.1")

  lazy val MRSCProject = Project("mrsc", file("src/mrsc"),
    settings = Project.defaultSettings ++ Seq(
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test",
      libraryDependencies += "junit" % "junit" % "4.8.1" % "test"
    )
  )

  lazy val SamplesProject = Project("samples", file("src/samples")) dependsOn(MRSCProject)
}
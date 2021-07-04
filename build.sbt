lazy val commonSettings = Seq(
    scalaVersion := "3.0.0",
    organization := "mrsc",
    version := "0.5.4",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    fork  := true,
    Test / baseDirectory in run := file("."),
    Test / testOptions += Tests.Argument("-oD"),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
)

lazy val MRSCCore = (project in file("mrsc-core"))
  .settings(commonSettings)
  .settings(
    name := "mrsc-core",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  )

lazy val MRSCCounters = (project in file("mrsc-counters"))
  .settings(commonSettings)
  .settings(
    name := "mrsc-counters",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  )
  .dependsOn(MRSCCore)

lazy val MRSCPfp = (project in file("mrsc-pfp"))
  .settings(commonSettings)
  .settings(
    name := "mrsc-pfp",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
    libraryDependencies += ("org.scalaz" %% "scalaz-core" % "7.3.3").cross(CrossVersion.for3Use2_13),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    unmanagedBase := file("lib"),
    Test / baseDirectory := file("."),
  )
  .dependsOn(MRSCCore % "compile->compile;test->test")

lazy val MRSCSamples = (project in file("mrsc-samples"))
  .settings(commonSettings)
  .settings(
    name := "mrsc-samples",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
    libraryDependencies += ("org.scalaz" %% "scalaz-core" % "7.3.3").cross(CrossVersion.for3Use2_13),
    fork := true,
    baseDirectory in run := file("."),
  )
  .dependsOn(MRSCCore % "test->test", MRSCPfp % "compile->compile,test->test")

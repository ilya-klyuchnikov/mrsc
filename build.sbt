lazy val MRSCCore = (project in file("mrsc-core"))
  .settings(
    organization := "mrsc",
    name := "mrsc-core",
    version := "0.5.3",
    scalaVersion := "2.13.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    fork := true,
    Test / baseDirectory in run := file("."),
    Test / testOptions += Tests.Argument("-oD"),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
  )

lazy val MRSCCounters = (project in file("mrsc-counters"))
  .settings(
    organization := "mrsc",
    name := "mrsc-counters",
    version := "0.5.3",
    scalaVersion := "2.13.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    fork := true,
    Test / baseDirectory in run := file("."),
    Test / testOptions += Tests.Argument("-oD")
  )
  .dependsOn(MRSCCore)

lazy val MRSCPfp = (project in file("mrsc-pfp"))
  .settings(
    organization := "mrsc",
    name := "mrsc-pfp",
    version := "0.5.3",
    scalaVersion := "2.13.1",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.29",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    unmanagedBase := file("lib"),
    fork := true,
    Test / baseDirectory := file("."),
    Test / testOptions += Tests.Argument("-oD")
  )
  .dependsOn(MRSCCore % "compile->compile;test->test")

lazy val MRSCSamples = (project in file("mrsc-samples"))
  .settings(
    organization := "mrsc",
    name := "mrsc-samples",
    version := "0.5.3",
    scalaVersion := "2.13.1",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.29",
    fork := true,
    Test / baseDirectory in run := file(".")
  )
  .dependsOn(MRSCCore % "test->test", MRSCPfp % "compile->compile,test->test")

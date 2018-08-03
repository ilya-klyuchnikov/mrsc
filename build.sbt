lazy val MRSCCore = (project in file("mrsc-core"))
  .settings(
    organization := "mrsc",
    name := "mrsc-core",
    version := "0.5.2",
    scalaVersion := "2.12.6",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    fork := true,
    Test / baseDirectory in run := file("."),
    Test / testOptions += Tests.Argument("-oD"),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
  )

lazy val MRSCCounters = (project in file("mrsc-counters"))
  .settings(
    organization := "mrsc",
    name := "mrsc-counters",
    version := "0.5.2",
    scalaVersion := "2.12.6",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    fork := true,
    Test / baseDirectory in run := file("."),
    Test / testOptions += Tests.Argument("-oD")
  )
  .dependsOn(MRSCCore)

lazy val MRSCPfp = (project in file("mrsc-pfp"))
  .settings(
    organization := "mrsc",
    name := "mrsc-pfp",
    version := "0.5.2",
    scalaVersion := "2.12.6",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.13",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
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
    version := "0.5.2",
    scalaVersion := "2.12.6",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.13",
    fork := true,
    Test / baseDirectory in run := file(".")
  )
  .dependsOn(MRSCCore % "test->test", MRSCPfp % "compile->compile,test->test")

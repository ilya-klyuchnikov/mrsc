lazy val MRSCCore = Project("mrsc-core", file("mrsc-core"),
  settings = Project.defaultSettings ++ Seq(
    organization := "mrsc",
    name := "mrsc-core",
    version := "0.5.1",
    scalaVersion := "2.11.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test",
    //fork := true,
    baseDirectory in run := file("."),
    testOptions in Test += Tests.Argument("-oD"),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
  ) ++ bintrayPublishSettings
)

lazy val MRSCCounters = Project("mrsc-counters", file("mrsc-counters"),
  settings = Project.defaultSettings ++ Seq(
    organization := "mrsc",
    name := "mrsc-counters",
    version := "0.5.1",
    scalaVersion := "2.11.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test",
    //fork := true,
    baseDirectory in run := file("."),
    testOptions in Test += Tests.Argument("-oD")
  )
) dependsOn(MRSCCore)

lazy val MRSCPfp = Project("mrsc-pfp", file("mrsc-pfp"),
  settings = Project.defaultSettings ++ Seq(
    organization := "mrsc",
    name := "mrsc-pfp",
    version := "0.5.1",
    scalaVersion := "2.11.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test",
    unmanagedBase := file("lib"),
    //fork := true,
    baseDirectory in run := file("."),
    testOptions in Test += Tests.Argument("-oD")
  )
) dependsOn(MRSCCore % "compile->compile;test->test")

lazy val MRSCSamples = Project("mrsc-samples", file("mrsc-samples"),
  settings = Project.defaultSettings ++ Seq(
    organization := "mrsc",
    name := "mrsc-samples",
    version := "0.5.1",
    scalaVersion := "2.11.0",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6",
    //fork := true,
    baseDirectory in run := file(".")
  )
) dependsOn(MRSCCore % "test->test", MRSCPfp % "compile->compile,test->test")

lazy val MRSCSamplesArrays = Project("mrsc-samples-arrays", file("mrsc-samples-arrays"),
  settings = Project.defaultSettings ++ Seq(
    organization := "mrsc",
    name := "mrsc-samples-arrays",
    version := "0.5.1",
    scalaVersion := "2.11.0",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6",
    //fork := true,
    baseDirectory in run := file(".")
  )
) dependsOn(MRSCPfp % "compile->compile,test->test")

lazy val root =
  Project(id = "parent", base = file(".")) aggregate(MRSCCore, MRSCCounters, MRSCPfp, MRSCSamples, MRSCSamplesArrays)
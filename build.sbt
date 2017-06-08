lazy val MRSCCore = Project("mrsc-core", file("mrsc-core"),
  settings = Project.defaultSettings ++ Seq(
    organization := "mrsc",
    name := "mrsc-core",
    version := "0.5.2",
    scalaVersion := "2.12.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    //fork := true,
    baseDirectory in run := file("."),
    testOptions in Test += Tests.Argument("-oD"),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
  )
)

lazy val MRSCCounters = Project("mrsc-counters", file("mrsc-counters"),
  settings = Project.defaultSettings ++ Seq(
    organization := "mrsc",
    name := "mrsc-counters",
    version := "0.5.2",
    scalaVersion := "2.12.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    //fork := true,
    baseDirectory in run := file("."),
    testOptions in Test += Tests.Argument("-oD")
  )
) dependsOn(MRSCCore)

lazy val MRSCPfp = Project("mrsc-pfp", file("mrsc-pfp"),
  settings = Project.defaultSettings ++ Seq(
    organization := "mrsc",
    name := "mrsc-pfp",
    version := "0.5.2",
    scalaVersion := "2.12.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.13",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
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
    version := "0.5.2",
    scalaVersion := "2.12.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.13",
    //fork := true,
    baseDirectory in run := file(".")
  )
) dependsOn(MRSCCore % "test->test", MRSCPfp % "compile->compile,test->test")

lazy val root =
  Project(id = "parent", base = file(".")) aggregate(MRSCCore, MRSCCounters, MRSCPfp, MRSCSamples)

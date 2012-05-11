scalaVersion := "2.9.1"

name := "mrsc"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

seq(ScctPlugin.scctSettings: _*)

resolvers ++= Seq(Classpaths.typesafeResolver, "sbt-idea-repo" at "http://mpeltonen.github.com/maven/")

addSbtPlugin("ch.craven" %% "scct-plugin" % "0.2")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")
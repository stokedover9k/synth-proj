name := "SynthProj"

scalaVersion := "2.10.2"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq(
	  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
	  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
		    "org.specs2" %% "specs2" % "2.2" % "test"
		    , "net.sf.opencsv" % "opencsv" % "2.3"
        , "org.scala-lang" % "scala-swing" % "2.10.2"
)

fork in run := true

connectInput in run := true


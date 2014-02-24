name := "expressions-scala"

version := "0.0.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

seq(bintrayResolverSettings:_*)

libraryDependencies ++= Seq(
  "edu.luc.etl" %% "scalamu" % "0.2.0",
  "org.scalatest" % "scalatest_2.10" % "2.0.1-SNAP" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.1" % "test"
)
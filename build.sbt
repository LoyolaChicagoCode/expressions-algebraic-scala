name := "expressions-algebraic-scala"

version := "0.0.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

seq(bintrayResolverSettings:_*)

libraryDependencies ++= Seq(
  "edu.luc.etl" %% "scalamu" % "0.2.2",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.5" % "test"
)

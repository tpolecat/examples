name := "examples"

version := "0.1"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  // Main
  "org.scalaz" % "scalaz-core_2.10" % "7.0.6",
  "org.scalaz" % "scalaz-effect_2.10" % "7.0.6",
  "org.scalaz" % "scalaz-concurrent_2.10" % "7.0.6",
  "io.argonaut" %% "argonaut" % "6.0-M3",
  "org.scala-stm" %% "scala-stm" % "0.7",
  // Test
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.specs2" %% "specs2" % "1.13" % "test"
)






import AssemblyKeys._

assemblySettings

name := "left-corner-dmv"

organization := "kmcs.nii"

version := "0.4"

// scalaVersion := "2.11.3"
scalaVersion := "2.11.6"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc(),
  // "org.scalatest" % "scalatest_2.10" % "2.1.7" % "test" withSources() withJavadoc(),
  // "org.scalacheck" %% "scalacheck" % "1.10.0" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test" withSources() withJavadoc(),
  "org.scalanlp" %% "breeze" % "0.10",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  "org.scalanlp" %% "breeze-natives" % "0.10",
  // "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  "org.slf4j" % "slf4j-simple" % "1.7.6",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "org.scala-lang.modules" %% "scala-xml" %  "1.0.1"
)

scalacOptions ++= Seq()

// initialCommands := "import kmcs.nii.leftcornerdmv._"

mainClass in assembly := Some("dmv.Driver")

// javacOptions ++= Seq("-Xlint:all")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ypatmat-exhaust-depth", "off"
)

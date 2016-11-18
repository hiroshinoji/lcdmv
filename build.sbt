import AssemblyKeys._

assemblySettings

name := "lcdmv"

organization := "mynlp"

version := "0.1"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test" withSources() withJavadoc(),
  "org.scalanlp" %% "breeze" % "0.10",
  "org.scalanlp" %% "breeze-natives" % "0.10",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  "org.slf4j" % "slf4j-simple" % "1.7.6",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "org.scala-lang.modules" %% "scala-xml" %  "1.0.1"
)

scalacOptions ++= Seq()

mainClass in assembly := Some("lcdmv.Driver")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ypatmat-exhaust-depth", "off"
)

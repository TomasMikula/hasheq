name := "hasheq"

version := "0.3"

organization := "com.github.tomasmikula"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

scalacOptions ++= Seq(
  "-Xlint",
  "-unchecked",
  "-deprecation",
  "-feature",
  //"-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ypatmat-exhaust-depth", "40",
  "-Xfuture")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

// send test results back to sbt (-o), display all durations (D),
// and full stack traces (F)
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")

fork in Test := false // true causes NotSerializableException

tutSettings

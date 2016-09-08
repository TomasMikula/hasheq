name := "hasheq"

version := "0.1"

organization := "com.github.tomasmikula"

scalaVersion := "2.11.8"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.2")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.15")

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

javacOptions ++= Seq(
  "-source", "1.8",
  "-target", "1.8",
  "-Xlint:unchecked",
  "-Xlint:deprecation")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

// send test results back to sbt (-o), display all durations (D),
// and full stack traces (F)
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")

fork in Test := false // true causes NotSerializableException

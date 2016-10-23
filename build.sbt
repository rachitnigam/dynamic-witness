name := "Dynamic-witness"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xfatal-warnings"
)

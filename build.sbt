name := "Dynamic-witness"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.0.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "com.github.scopt" %% "scopt" % "3.5.0"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Yno-adapted-args",
  "-Xfatal-warnings"
)

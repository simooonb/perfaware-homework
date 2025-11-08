ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

scalacOptions ++= Seq("-Xfatal-warnings")

lazy val root = (project in file("."))
  .settings(
    name := "perfaware"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

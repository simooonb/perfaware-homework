ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.17"

scalacOptions ++= Seq("-Xfatal-warnings")

lazy val root = (project in file("."))
  .settings(
    name := "perfaware"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

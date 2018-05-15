name := "AOC-2017"

scalaVersion := "2.12.6"
scalacOptions ++= Seq("-deprecation", "-feature")

excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test


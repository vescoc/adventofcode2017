name := "AOC-2017"

scalaVersion := "2.12.4"
scalacOptions ++= Seq("-deprecation", "-feature")

excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"


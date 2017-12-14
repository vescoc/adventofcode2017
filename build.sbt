name := "AOC-2017"

scalaVersion := "2.12.4"
scalacOptions ++= Seq("-deprecation")

excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"


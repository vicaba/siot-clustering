name := "siot-clustering"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.apache.commons" % "commons-csv" % "1.4"
libraryDependencies += "com.twitter" %% "algebird-core" % "0.13.4"
libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2"
libraryDependencies += "com.typesafe" % "config" % "1.3.3"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"


libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

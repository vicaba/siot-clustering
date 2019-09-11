import sbt._

object Dependencies {

  val akkaVersion = "2.5.17"

  val apacheCommonsCsv = "org.apache.commons" % "commons-csv" % "1.4"

  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion

  val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion

  val breeze = "org.scalanlp" %% "breeze" % "0.13.2"

  val typesafeConfig = "com.typesafe" % "config" % "1.3.3"

  val playJson = "com.typesafe.play" %% "play-json" % "2.6.7"

  val typesafeScalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"

  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val scalaFx = "org.scalafx" %% "scalafx" % "8.0.181-R13"

  val scalactic = "org.scalactic" %% "scalactic" % "3.0.5"

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"

  val scalameter = "com.storm-enroute" %% "scalameter-core" % "0.19" % "test"

}

import sbt.Keys.packageBin

name := "siot-clustering"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(JavaAppPackaging)

lazy val deployTask = TaskKey[Unit]("deploy", "Copies assembly jar to remote location")

deployTask := (Universal / packageBin map { _packageBin =>
  val account = "vcaballero@172.16.2.172" // FIXME!
val local   = _packageBin.getPath
  val remote  = account + ":" + "/" + _packageBin.getName
  println(s"Copying: $local -> $account:$remote")
  Seq("scp", local, remote)
}).value

val helloTask = TaskKey[Unit]("hello", "Print hello")

helloTask := println("hello world")

libraryDependencies += "org.apache.commons" % "commons-vfs2" % "2.2"

libraryDependencies += "org.apache.commons" % "commons-csv" % "1.4"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.13"
libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2"
libraryDependencies += "com.typesafe" % "config" % "1.3.3"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.181-R13"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.17",
)


libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"



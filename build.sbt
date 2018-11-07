import deployssh.DeploySSH.ServerConfig
import sbt.Keys.packageBin

name := "siot-clustering"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(JavaAppPackaging)

lazy val myProject = project.enablePlugins(DeploySSH).settings(
  //load build.conf from external path
  deployExternalConfigFiles ++= Seq("/home/myUser/Documents/build.conf"),
  //load build2.conf from `myProjectDir` and load build3.conf from `myProjectDir/project`
  deployResourceConfigFiles ++= Seq("build2.conf", "project/build3.conf"),
  //load build4.conf from user home directory (in example `/home/myUser/build4.conf`)
  deployHomeConfigFiles ++= Seq("build4.conf"),
  //configuration in project setttings
  deployConfigs ++= mySettings,
  deployConfigs ++= Seq(
    ServerConfig("server_6", "169.254.0.3"),
    ServerConfig("server_7", "169.254.0.4")
  )
  )

lazy val mySettings = Seq(
  ServerConfig("server_5", "169.254.0.2")
)

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



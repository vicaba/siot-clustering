import sbt.Keys._
import Dependencies._

name := "siot-clustering"

version := "0.1"

scalaVersion := "2.12.8"

resourceDirectory in Test := baseDirectory.value / "test-resources"

enablePlugins(JavaAppPackaging)

lazy val deployTask = TaskKey[Unit]("deploy", "Copies assembly jar to remote location")

deployTask := (Universal / packageBin map { _packageBin =>
  val username   = "vcaballero"
  val password   = "vcaballero"
  val remoteHost = "172.16.2.253"
  val localPath  = _packageBin.getPath
  val remotePath = "/home/vcaballero/" + _packageBin.getName

  SftpUtility.upload(remoteHost, username, password, localPath, remotePath)

}).value

val helloTask = TaskKey[Unit]("hello", "Print hello")

helloTask := println("hello world")

libraryDependencies ++= Seq(
  akkaActor,
  akkaStream,
  breeze,
  apacheCommonsCsv,
  playJson,
  typesafeConfig,
  typesafeScalaLogging,
  logback,
  scalaFx,
  scalactic,
  scalaTest
)

parallelExecution in Test := false

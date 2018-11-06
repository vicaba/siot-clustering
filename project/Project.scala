import sbt._
import Keys.{packageBin, _}

object ScpDeploy {

  val deployTask = TaskKey[Unit]("deploy", "Copies assembly jar to remote location")

  deployTask := packageBin map { packageBin =>
    val account = "user@example.com" // FIXME!
    val local   = packageBin.getPath
    val remote  = account + ":" + "/tmp/" + packageBin.getName
    println(s"Copying: $local -> $account:$remote")
    Seq("scp", local, remote)
  }

}
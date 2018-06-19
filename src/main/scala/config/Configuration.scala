package config

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

trait Configuration {

  val userProfilesFile: String

}

object Configuration extends Configuration  {

  private lazy val conf: Config = ConfigFactory.load("Application.conf")

  override val userProfilesFile: String = conf.getString("userEnergyProfiles.inFile")

}

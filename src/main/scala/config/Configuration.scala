package config

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

trait Configuration {

  val userProfilesFile: String

  val clustererFile: String

  val reschedulerFile: String

  val batchRunFile: String

  val summaryBatchRunFile: String

}

object Configuration extends Configuration  {

  private lazy val conf: Config = ConfigFactory.load("Application.conf")

  override val userProfilesFile: String = conf.getString("userEnergyProfiles.inFile")

  override val clustererFile: String = conf.getString("output.clustererFile")

  override val reschedulerFile: String = conf.getString("output.reschedulerFile")

  override val batchRunFile: String = conf.getString("output.batchRunFile")

  override val summaryBatchRunFile: String = conf.getString("output.summaryBatchRunFile")

}

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

object Configuration extends Configuration {

  private lazy val conf: Config = ConfigFactory.load("application.conf")

  override val userProfilesFile: String = conf.getString("user-energy-profiles.in-file")

  override val clustererFile: String = conf.getString("output.clusterer-file")

  override val reschedulerFile: String = conf.getString("output.rescheduler-file")

  override val batchRunFile: String = conf.getString("output.batch-run-file")

  override val summaryBatchRunFile: String = conf.getString("output.summary-batch-run-file")

  object BatchRun {

    object KRange {

      val from: Int = conf.getInt("batch-run.k-range.from")

      val to: Int = conf.getInt("batch-run.k-range.to")

    }

  }

  object CrossFold {

    val _type: String  = conf.getString("cross-fold.type")

    val splits: String = conf.getString("cross-fold.splits")

    object SubsampleSize {

      val from: Double = conf.getDouble("cross-fold.subsample-size.from")

      val to: Double = conf.getDouble("cross-fold.subsample-size.to")

    }
  }

}

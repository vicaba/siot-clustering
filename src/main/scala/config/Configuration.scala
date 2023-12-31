package config

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

trait Configuration {

  val userProfilesFile: String

  val clustererFile: String

  val reschedulerFile: String

  val batchRunFile: String

  val summaryBatchRunFile: String

  val configFile: String

  val releaseBaseFile: String

}

object Configuration extends Configuration {

  lazy val conf: Config = ConfigFactory.load("application.conf")

  override val userProfilesFile: String = conf.getString("user-energy-profiles.in-file")

  override val clustererFile: String = conf.getString("output.clusterer-file")

  override val reschedulerFile: String = conf.getString("output.rescheduler-file")

  override val batchRunFile: String = conf.getString("output.batch-run-file")

  override val summaryBatchRunFile: String = conf.getString("output.summary-batch-run-file")

  override val configFile: String = conf.getString("output.config-file")

  override val releaseBaseFile: String = conf.getString("release.base")


  object ClusteringAlgorithm {

    object LeafEnergyConsumption {
      val pattern = """m(\d)""".r

      def apply(averageEnergyConsumption: String): Double => Double = averageEnergyConsumption match {
        case pattern(times) => (n1: Double) => n1 * times.toInt
      }
    }

    val leafEnergyConsumption: Double => Double = LeafEnergyConsumption(conf.getString("clustering-algorithm.parameters.leaf-energy-consumption"))
  }

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

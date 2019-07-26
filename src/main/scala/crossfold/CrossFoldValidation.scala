package crossfold

import algorithm.BatchRunSettingsBuilder
import algorithm.EuclideanAlgorithm.{ClustererAndReschedulerOutput, ClustererOutput}
import batch.GenBatchRun
import com.typesafe.scalalogging.Logger

import scala.util.Random

object CrossFoldValidation {

  val logger = Logger("CrossfoldValidation")


  case class Percentage private (v: BigDecimal)

  /**
    *  Range [0, 1]
    */
  object Percentage {
    def of(v: BigDecimal): Percentage = {
      if (v > 1) Percentage(1)
      else if (v < 0) Percentage(0)
      else Percentage(v)
    }
  }

  sealed trait CrossFoldTypeSettings

  case class MonteCarlo(splits: Int, subSampleSize: Percentage) extends CrossFoldTypeSettings

  /**
    * Creates "splits" number of BatchRunSettingsBuilder taking a subsampleSize percentage of the total points
    * @param settings
    * @param batchRunSettings
    * @return
    */
  def runClusterer(settings: CrossFoldTypeSettings,
                                   batchRunSettings: BatchRunSettingsBuilder)
    : List[List[ClustererOutput]] = settings match {
    case s: MonteCarlo =>
      val points = batchRunSettings.points
      val splits = for (i <- 0 until s.splits) yield {
        Random.shuffle(points).take(Math.floor((points.size * s.subSampleSize.v).toDouble).toInt)
      }
      val bulkBatchRunSettings = splits.map(p => batchRunSettings.copy(points = p))
      bulkBatchRunSettings.zipWithIndex.map { case (builder, idx) =>
        logger.info("Split: {}.", idx)
        GenBatchRun.cluster(builder.build.map(_._1))
      }.toList

  }

  /**
    * For each CrossFoldTypeSettings (that indicates the number of splits and the subSample size), create "splits" number
    * of BatchRunSettingsBuilder taking a subSampleSize percentage of the total points
    * @param settings
    * @param batchRunSettings
    * @return
    */
  def batchRunClusterer(settings: List[CrossFoldTypeSettings],
                                        batchRunSettings: BatchRunSettingsBuilder)
    : List[(CrossFoldValidation.CrossFoldTypeSettings, List[List

    [ClustererOutput]])] =
    settings.map(s => (s, runClusterer(s, batchRunSettings)))

  def run(settings: CrossFoldTypeSettings,
                                   batchRunSettings: BatchRunSettingsBuilder)
  : List[List[ClustererAndReschedulerOutput]] = settings match {
    case s: MonteCarlo =>
      val points = batchRunSettings.points
      val splits = for (i <- 0 until s.splits) yield {
        Random.shuffle(points).take(Math.floor((points.size * s.subSampleSize.v).toDouble).toInt)
      }
      val bulkBatchRunSettings = splits.map(p => batchRunSettings.copy(points = p))
      bulkBatchRunSettings.zipWithIndex.map { case (builder, idx) =>
        logger.info("Split: {}.", idx)
        GenBatchRun.apply(builder.build)
      }.toList
  }

  def batchRun(settings: List[CrossFoldTypeSettings],
                                        batchRunSettings: BatchRunSettingsBuilder)
  : List[(CrossFoldValidation.CrossFoldTypeSettings, List[List[ClustererAndReschedulerOutput]])] =
    settings.map(s => (s, run(s, batchRunSettings)))

}

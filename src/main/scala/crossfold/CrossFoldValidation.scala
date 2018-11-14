package crossfold
import algorithm.algorithms.GenAlgorithm
import algorithm.algorithms.euclidean.BatchRunSettingsBuilder
import batch.GenBatchRun

import scala.collection.immutable
import scala.util.Random

object CrossFoldValidation {

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

  case class LeavePOut(p: Int) extends CrossFoldTypeSettings

  case class KFold(k: Int) extends CrossFoldTypeSettings

  case class MonteCarlo(splits: Int, subsampleSize: Percentage) extends CrossFoldTypeSettings

  def run(algorithm: GenAlgorithm)(settings: CrossFoldTypeSettings,
                                   batchRunSettings: algorithm.BatchRunSettingsBuilderT)
    : List[List[algorithm.type#StepT[algorithm.type#ClustererSettingsT]]] = settings match {
    case s: MonteCarlo =>
      val points = batchRunSettings.points
      val splits = for (i <- 0 until s.splits) yield {
        Random.shuffle(points).take(Math.floor((points.size * s.subsampleSize.v).toDouble).toInt)
      }
      val bulkBatchRunSettings = splits.map(p => batchRunSettings.copy(points = p))
      bulkBatchRunSettings.map { builder => GenBatchRun.cluster(algorithm)(builder.build.map(_._1))
      }.toList

  }

  def batchRun(algorithm: GenAlgorithm)(settings: List[CrossFoldTypeSettings],
                                        batchRunSettings: algorithm.BatchRunSettingsBuilderT)
    : List[(CrossFoldValidation.CrossFoldTypeSettings, List[List[algorithm.StepT[algorithm.ClustererSettingsT]]])] =
    settings.map(s => (s, run(algorithm)(s, batchRunSettings)))

}

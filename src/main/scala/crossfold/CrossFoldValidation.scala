package crossfold
import algorithm.algorithms.GenAlgorithm
import algorithm.algorithms.euclidean.BatchRunSettingsBuilder
import batch.GenBatchRun

import scala.util.Random

object CrossFoldValidation {

  sealed trait CrossFoldTypeSettings

  case class LeavePOut(p: Int) extends CrossFoldTypeSettings

  case class KFold(k: Int)

  case class MonteCarlo()

  def run(algorithm: GenAlgorithm)(settings: CrossFoldTypeSettings,
                                   batchRunSettings: algorithm.BatchRunSettingsBuilderT) = settings match {
    case s: KFold =>
      val points               = batchRunSettings.points
      val splits               = Random.shuffle(points).grouped(s.k)
      val trainingSets         = for (i <- 0 until splits.size) yield { splits.zipWithIndex.filterNot(_._2 == i).flatMap(_._1) }
      val bulkBatchRunSettings = trainingSets.map(p => batchRunSettings.copy(points = p.toVector))
      bulkBatchRunSettings.map { builder =>
        GenBatchRun.cluster(algorithm)(builder.build.map(_._1))
      }
  }

}

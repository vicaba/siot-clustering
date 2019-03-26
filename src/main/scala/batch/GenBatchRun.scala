package batch

import algorithm.algorithms.euclidean.EuclideanAlgorithm.{ClustererAndReschedulerOutput, ReschedulerSettingsT}
import algorithm.algorithms.euclidean.EuclideanAlgorithm

object GenBatchRun {

  def apply(settings: List[(EuclideanAlgorithm.ClustererSettingsT, EuclideanAlgorithm.ReschedulerSettingsT)])
    : List[ClustererAndReschedulerOutput] = {
    settings.map {
      case (clustererSettings, reschedulerSettings) =>
        EuclideanAlgorithm.apply(clustererSettings, reschedulerSettings)
    }
  }

  def cluster(settings: List[EuclideanAlgorithm.ClustererSettingsT]): List[EuclideanAlgorithm.ClustererOutput] = {
    settings.map { clustererSettings =>
      EuclideanAlgorithm.apply(clustererSettings)
    }
  }

}

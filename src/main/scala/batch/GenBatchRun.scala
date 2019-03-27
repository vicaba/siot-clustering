package batch
import algorithm.EuclideanAlgorithm
import algorithm.EuclideanAlgorithm.{ClustererAndReschedulerOutput, ReschedulerSettingsT}

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

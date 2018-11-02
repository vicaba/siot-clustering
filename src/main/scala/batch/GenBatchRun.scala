package batch
import algorithm.algorithms.GenAlgorithm

object GenBatchRun {

  def apply(algorithm: GenAlgorithm)(settings: List[(algorithm.type#ClustererSettingsT, algorithm.type#ReschedulerSettingsT)]): List[algorithm.type#Steps] = {
    settings.map { case (clustererSettings, reschedulerSettings) =>
      algorithm.apply(clustererSettings, reschedulerSettings)
    }
  }

  def cluster(algorithm: GenAlgorithm)(settings: List[algorithm.type#ClustererSettingsT]): List[algorithm.type#StepT[algorithm.type#ClustererSettingsT]] = {
    settings.map { clustererSettings =>
      algorithm.apply(clustererSettings)
    }
  }

}

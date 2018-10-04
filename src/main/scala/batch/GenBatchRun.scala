package batch
import algorithm.algorithms.GenAlgorithm

object GenBatchRun {

  def apply(algorithm: GenAlgorithm)(settings: List[(algorithm.type#ClustererSettings, algorithm.type#ReschedulerSettings)]): List[algorithm.type#Steps] = {
    settings.map { case (clustererSettings, reschedulerSettings) =>
      algorithm.apply(clustererSettings, reschedulerSettings)
    }
  }

  def cluster(algorithm: GenAlgorithm)(settings: List[algorithm.type#ClustererSettings]): List[algorithm.type#StepT[algorithm.type#ClustererSettings]] = {
    settings.map { clustererSettings =>
      algorithm.apply(clustererSettings)
    }
  }

}

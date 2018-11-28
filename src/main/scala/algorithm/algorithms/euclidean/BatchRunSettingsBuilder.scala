package algorithm.algorithms.euclidean


import algorithm.clusterer.EuclideanClusterer
import metrics.Metric
import types.immutable.Point
class BatchRunSettingsBuilder(override val points: Vector[Point],
                              override val numbersOfClusters: List[Int],
                              override val metrics: List[Metric],
                              override val improveIterations: (Vector[Point], Int) => Int)
    extends algorithm.algorithms.BatchRunSettingsBuilder[EuclideanAlgorithm.type] {

  override def copy(points: Vector[Point],
                    numbersOfClusters: List[Int],
                    metrics: List[Metric],
                    timesToIterate: (Vector[Point], Int) => Int): BatchRunSettingsBuilder =
    new BatchRunSettingsBuilder(points, numbersOfClusters, metrics, timesToIterate)

  override def build: List[(EuclideanClusterer.Settings, algorithm.scheduler.Settings)] = {
    numbersOfClusters.flatMap { numberOfClusters =>
      metrics.map { metric =>
        (EuclideanClusterer.Settings(numberOfClusters,
                                              points,
                                              metric,
                                              improveIterations(points, numberOfClusters)),
          algorithm.scheduler.Settings(numberOfClusters, metric, 0.1, memory = 3))
      }
    }
  }
}

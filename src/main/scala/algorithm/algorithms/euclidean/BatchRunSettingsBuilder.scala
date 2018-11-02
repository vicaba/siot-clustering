package algorithm.algorithms.euclidean

import algorithm.clusterer.FlattenedEuclideanClusterer
import algorithm.scheduler.ClusterRescheduler
import metrics.Metric
import types.Point

class BatchRunSettingsBuilder(override val points: Vector[Point],
                              override val numbersOfClusters: List[Int],
                              override val metrics: List[Metric],
                              override val timesToIterate: (Vector[Point], Int) => Int)
    extends algorithm.algorithms.BatchRunSettingsBuilder[EuclideanAlgorithm.type] {

  override def copy(points: Vector[Point],
                    numbersOfClusters: List[Int],
                    metrics: List[Metric],
                    timesToIterate: (Vector[Point], Int) => Int): BatchRunSettingsBuilder =
    new BatchRunSettingsBuilder(points, numbersOfClusters, metrics, timesToIterate)

  override def build: List[(FlattenedEuclideanClusterer.Settings, ClusterRescheduler.Settings)] = {
    numbersOfClusters.flatMap { numberOfClusters =>
      metrics.map { metric =>
        (FlattenedEuclideanClusterer.Settings(numberOfClusters,
                                              points,
                                              metric,
                                              timesToIterate(points, numberOfClusters)),
         ClusterRescheduler.Settings(metric, 0.5, memory = 3))
      }
    }
  }
}

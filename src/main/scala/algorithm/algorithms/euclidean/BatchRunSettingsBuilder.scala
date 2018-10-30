package algorithm.algorithms.euclidean

import algorithm.clusterer.FlattenedEuclideanClusterer
import algorithm.scheduler.ClusterRescheduler
import metrics.Metric
import types.Point

class BatchRunSettingsBuilder(points: Vector[Point],
                              numbersOfClusters: List[Int],
                              metrics: List[Metric],
                              timesToIterate: (Vector[Point], Int) => Int) {

  def build: List[(FlattenedEuclideanClusterer.Settings, ClusterRescheduler.Settings)] =
    numbersOfClusters.flatMap { numberOfClusters =>
      metrics.map { metric =>
        (FlattenedEuclideanClusterer.Settings(numberOfClusters, points, metric, timesToIterate(points, numberOfClusters)),
         ClusterRescheduler.Settings(metric, 0.5, memory = 3))
      }
    }
}

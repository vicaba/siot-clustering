package algorithm.algorithms.euclidean

import algorithm.clusterer.EuclideanClusterer
import algorithm.scheduler.ClusterRescheduler
import metrics.Metric
import types.Point

class BatchRunSettingsBuilder(points: Vector[Point],
                              numbersOfClusters: List[Int],
                              metrics: List[Metric],
                              timesToIterate: (Vector[Point], Int) => Int) {

  def build: List[(EuclideanClusterer.Settings, ClusterRescheduler.Settings)] =
    numbersOfClusters.flatMap { numberOfClusters =>
      metrics.map { metric =>
        (EuclideanClusterer.Settings(numberOfClusters, points, metric, timesToIterate(points, numberOfClusters)),
         ClusterRescheduler.Settings(metric, 0.5, memory = 3))
      }
    }
}

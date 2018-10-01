package batch

import algorithm.clusterer.BruteClusterer
import algorithm.scheduler.ClusterRescheduler
import metrics.Metric
import types.Point

class BatchRunSettingsBuilder(points: Vector[Point],
                              numbersOfClusters: List[Int],
                              metrics: List[Metric],
                              timesToIterate: (Vector[Point], Int) => Int) {

  def build: List[(BruteClusterer.Settings, ClusterRescheduler.Settings)] =
    numbersOfClusters.flatMap { numberOfClusters =>
      metrics.map { metric =>
        (BruteClusterer.Settings(numberOfClusters, points, metric, timesToIterate(points, numberOfClusters)),
         ClusterRescheduler.Settings(metric, 0.5, memory = 3))
      }
    }

}

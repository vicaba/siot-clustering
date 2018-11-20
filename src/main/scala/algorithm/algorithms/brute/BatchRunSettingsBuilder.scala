package algorithm.algorithms.brute

import algorithm.clusterer.BruteClusterer
import algorithm.scheduler.ClusterReschedulerOld
import metrics.Metric
import types.Point

class BatchRunSettingsBuilder(override val points: Vector[Point],
                              override val numbersOfClusters: List[Int],
                              override val metrics: List[Metric],
                              override val improveIterations: (Vector[Point], Int) => Int)
    extends algorithm.algorithms.BatchRunSettingsBuilder[BruteAlgorithm.type] {

  override def copy(points: Vector[Point],
                    numbersOfClusters: List[Int],
                    metrics: List[Metric],
                    timesToIterate: (Vector[Point], Int) => Int): BatchRunSettingsBuilder =
    new BatchRunSettingsBuilder(points, numbersOfClusters, metrics, timesToIterate)

  def build: List[(BruteClusterer.Settings, ClusterReschedulerOld.Settings)] =
    numbersOfClusters.flatMap { numberOfClusters =>
      metrics.map { metric =>
        (BruteClusterer.Settings(numberOfClusters, points, metric, improveIterations(points, numberOfClusters)),
         ClusterReschedulerOld.Settings(numberOfClusters, metric, 0.5, memory = 3))
      }
    }

}

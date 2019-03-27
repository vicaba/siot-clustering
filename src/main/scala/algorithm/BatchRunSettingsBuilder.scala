package algorithm

import algorithm.clusterer.EuclideanClusterer
import metrics.Metric
import types.immutable.Point

class BatchRunSettingsBuilder(val points: Vector[Point],
                              val numbersOfClusters: List[Int],
                              val metrics: List[Metric],
                              val timesToIterate: (Vector[Point], Int) => Int) {

  def copy(points: Vector[Point] = this.points,
           numbersOfClusters: List[Int] = this.numbersOfClusters,
           metrics: List[Metric] = this.metrics,
           timesToIterate: (Vector[Point], Int) => Int = this.timesToIterate): BatchRunSettingsBuilder =
    new BatchRunSettingsBuilder(points, numbersOfClusters, metrics, timesToIterate)

  def build: List[(EuclideanClusterer.Settings, algorithm.scheduler.ReschedulerSettings)] = {
    numbersOfClusters.flatMap { numberOfClusters =>
      metrics.map { metric =>
        (EuclideanClusterer.Settings(numberOfClusters, points, metric, timesToIterate(points, numberOfClusters)),
         algorithm.scheduler.ReschedulerSettings(numberOfClusters, metric, 0.1, memory = 3))
      }
    }
  }
}

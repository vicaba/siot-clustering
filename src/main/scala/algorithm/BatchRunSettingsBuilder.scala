package algorithm

import metrics.Metric
import test.{SchedulerAlgorithm, UserAllocator}
import test.reschedulermetrics.BiasedAverageDistanceTransformation
import types.clusterer.immutable.Point

/**
  * This class builds permutations of Clusterer and Rescheduler settings given a List containing the number of clusters
  * and a List containing the metrics to try.
  * @param points
  * @param numbersOfClusters
  * @param metrics
  * @param timesToIterate
  */
class BatchRunSettingsBuilder(val points: Vector[Point],
                              val numbersOfClusters: List[Int],
                              val metrics: List[Metric],
                              val timesToIterate: (Vector[Point], Int) => Int) {

  def copy(points: Vector[Point] = this.points,
           numbersOfClusters: List[Int] = this.numbersOfClusters,
           metrics: List[Metric] = this.metrics,
           timesToIterate: (Vector[Point], Int) => Int = this.timesToIterate): BatchRunSettingsBuilder =
    new BatchRunSettingsBuilder(points, numbersOfClusters, metrics, timesToIterate)

  def build: List[(clusterer.EuclideanClustererSettings, algorithm.scheduler.ReschedulerSettings)] = {
    numbersOfClusters.flatMap { numberOfClusters =>
      metrics.map { metric =>
        (clusterer.EuclideanClustererSettings(numberOfClusters,
                                              points,
                                              metric,
                                              timesToIterate(points, numberOfClusters)),
         algorithm.scheduler.ReschedulerSettings(numberOfClusters, metric, new BiasedAverageDistanceTransformation, UserAllocator.DefaultOrderings, SchedulerAlgorithm.DefaultOrderings)
      }
    }
  }
}

package batch

import java.io.PrintWriter

import algorithm.Algorithm.Steps
import algorithm._
import algorithm.clusterer.Clusterer
import algorithm.scheduler.ClusterRescheduler
import metrics.Metric
import types.Point

object BatchRun {

  class BatchRunSettingsBuilder(
    points: Vector[Point],
    numbersOfClusters: List[Int],
    metrics: List[Metric],
    timesToIterate: (Vector[Point], Int) => Int) {

    def build: List[Clusterer.Settings] =
      numbersOfClusters.flatMap { numberOfClusters =>
        metrics.map { metric =>
          Clusterer.Settings(numberOfClusters, points, metric, timesToIterate(points, numberOfClusters))
        }
      }

  }

  def apply(clustererSettingsBuilder: BatchRunSettingsBuilder): List[Steps] = {

    clustererSettingsBuilder.build.map { clustererSettings =>

      val reschedulerSettings = ClusterRescheduler.Settings(clustererSettings.metric, 0.5, memory = 3)

      val steps = Algorithm(clustererSettings, reschedulerSettings)

      steps
    }
  }

}

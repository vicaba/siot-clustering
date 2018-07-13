package batch

import java.io.PrintWriter

import algorithm.Algorithm.Steps
import algorithm._
import algorithm.clusterer.Clusterer
import algorithm.scheduler.ClusterRescheduler
import metrics.Metric
import types.Point

object BatchRunner {

  case class ClustererSettingsBuilder(points: Vector[Point], numbersOfClusters: List[Int], metrics: List[Metric], times: Vector[Point] => Int) {
    def build: List[Clusterer.Settings] =
      numbersOfClusters.flatMap { numberOfClusters =>
        metrics.map { metric =>
          Clusterer.Settings(numberOfClusters, points, metric, times(points))
        }
      }
  }

  def apply(clustererSettingsBuilder: ClustererSettingsBuilder): List[Steps] = {

    clustererSettingsBuilder.build.map { clustererSettings =>

      val reschedulerSettings = ClusterRescheduler.Settings(clustererSettings.metric, 0.5, memory = 2)

      val steps = Algorithm(clustererSettings, reschedulerSettings)

      steps
    }
  }

}

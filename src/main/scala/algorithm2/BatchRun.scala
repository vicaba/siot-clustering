package algorithm2

import java.io.PrintWriter

import algorithm2.Algorithm.Steps
import algorithm2._
import algorithm2.clusterer.Clusterer
import algorithm.scheduler.ClusterRescheduler
import com.typesafe.scalalogging.Logger
import metrics.Metric
import types.{Cluster, Point}

object BatchRun {

  val logger = Logger("batch")

  class BatchRunSettingsBuilder(points: Vector[Point],
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

  def apply(clustererSettingsBuilder: BatchRunSettingsBuilder,
            run: Clusterer.Settings => List[Cluster]): List[List[Cluster]] =
    clustererSettingsBuilder.build.map(run)

  def apply(clustererSettingsBuilder: BatchRunSettingsBuilder): List[Steps] = {

    clustererSettingsBuilder.build.map { clustererSettings =>
      logger.info("Running Batch with: {}", clustererSettings.toString)

      val reschedulerSettings = ClusterRescheduler.Settings(clustererSettings.metric, 0.5, memory = 3)

      val steps = Algorithm(clustererSettings, reschedulerSettings)

      steps
    }
  }

}

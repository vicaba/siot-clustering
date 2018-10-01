package algorithm2

import java.io.PrintWriter

import algorithm.algorithms.EuclideanAlgorithm
import algorithm.clusterer.EuclideanClusterer
import algorithm.algorithms.EuclideanAlgorithm.Steps
import algorithm2._
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

    def build: List[EuclideanClusterer.Settings] =
      numbersOfClusters.flatMap { numberOfClusters =>
        metrics.map { metric =>
          EuclideanClusterer.Settings(numberOfClusters, points, metric, timesToIterate(points, numberOfClusters))
        }
      }

  }

  def apply(clustererSettingsBuilder: BatchRunSettingsBuilder,
            run: EuclideanClusterer.Settings => List[Cluster]): List[List[Cluster]] =
    clustererSettingsBuilder.build.map(run)

  def apply(clustererSettingsBuilder: BatchRunSettingsBuilder): List[Steps] = {

    clustererSettingsBuilder.build.map { clustererSettings =>
      logger.info("Running Batch with: {}", clustererSettings.toString)

      val reschedulerSettings = ClusterRescheduler.Settings(clustererSettings.metric, 0.5, memory = 3)

      val steps = EuclideanAlgorithm(clustererSettings, reschedulerSettings)

      steps
    }
  }

}

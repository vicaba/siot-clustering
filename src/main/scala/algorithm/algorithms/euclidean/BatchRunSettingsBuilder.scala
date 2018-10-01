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

package main


import java.io.PrintWriter

import breeze.linalg.DenseVector
import clustering.Algorithm
import clustering.Algorithm.Run
import clustering.scheduler.ClusterRescheduler
import types._
import types.Types._
import config.Configuration
import metrics.Metric
import play.api.libs.json.Json
import reader.Reader
import types.Point
import types.serialization.ClusterJsonSerializer._

object Main {

  def main(args: Array[String]): Unit = {
    val algorithmBuilder = Algorithm
    val points = Reader.readUserRanges(Configuration.userProfilesFile).zipWithIndex.map { case (values, idx) =>
      val v = EmptyData()
      v(0, ::) := DenseVector[Double](values: _*).t
      Point(idx, v)
    }

    val run = Run(5, points, Metric.par, 0.5)
    val result = Algorithm.runIterative(run, 100)

    Some(new PrintWriter("clusters.json")).foreach{p =>
      p.write(Json.toJson(result).toString()); p.close
    }

    val rescheduledClusters = result.map { c =>
      ClusterRescheduler.rescheduleCluster(c, Metric.par, 0.5, 2)._1
    }

    Some(new PrintWriter("rescheduledClusters.json")).foreach{p =>
      p.write(Json.toJson(rescheduledClusters).toString()); p.close
    }

  }

}

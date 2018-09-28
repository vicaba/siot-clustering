package algorithm2
import java.io.PrintWriter

import breeze.linalg.{Vector, max}
import config.Configuration
import metrics.Par
import play.api.libs.json.{JsObject, JsValue, Json}
import types.{Cluster, Point, Types2}
import algorithm2.BatchRun.BatchRunSettingsBuilder
import algorithm2.clusterer.Clusterer.{chain, cluster}
import eventmanager.EventManager
import main.Main.{batchRun, readEgaugeData}
import util.FileUtils
import algorithm2.serialization.AlgorithmJsonSerializer._
import algorithm2.serialization.ClustererSettingsJsonSerializer._



object Main {

  def main(args: Array[String]): Unit = {
    val points = Generator
      .generateRandom2DPoints(Vector(0.0, 0.0), 5, 50, 5)
      .zipWithIndex
      .map {
        case (m, idx) =>
          Point(idx, m.toDenseVector.asDenseMatrix, None)(Types2)      }
      .toVector

    var clustersBuffer: List[List[Cluster]] = Nil
    EventManager.singleton
      .subscribe("clusters",
        (topic: String, event: Object) => clustersBuffer = event.asInstanceOf[List[Cluster]] :: clustersBuffer)

    /*    readEgaugeData("files/input/egauge.json").map { p =>
      Cluster(p.id, p.id.toString, Set(p))(p.types)
    }.toList.take(10)*/

    batchRun(points)

    Some(new PrintWriter("files/output/cluster.json")).foreach { p =>
      val json = clustersBuffer.zipWithIndex.map {
        case (clusteringIteration, idx) =>
          Json.obj(
            "iteration" -> idx,
            "clusters"  -> Json.toJson(clusteringIteration)
          )
      }
      p.write(Json.prettyPrint(Json.toJson(json)).toString)
      p.close()
    }

    FileUtils.copyFile("files/output/cluster.json",
      "/Users/vcaballero/Projects/jupyter-notebook/siot-eclustering-viz/files")

    FileUtils.copyFile(Configuration.summaryBatchRunFile, "/Users/vcaballero/Projects/jupyter-notebook/siot-eclustering-viz/files")
    FileUtils.copyFile(Configuration.batchRunFile, "/Users/vcaballero/Projects/jupyter-notebook/siot-eclustering-viz/files")

  }

  def batchRun(points: scala.Vector[Point]) = {

    val batchRunnerSettingsBuilder = new BatchRunSettingsBuilder(points,
      (1 to 6).toList,
      List(Par.withAverageAggregate),
      (points, k) => points.size * k)

    val stepsList = BatchRun(batchRunnerSettingsBuilder).zipWithIndex

    Some(new PrintWriter(Configuration.batchRunFile)).foreach { p =>
      val jsonList = stepsList.map {
        case (steps, idx) =>
          Json.obj(
            "run"   -> idx,
            "steps" -> Json.toJson(steps)
          )
      }

      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = stepsList.map {
        case (steps, idx) =>
          Json.obj(
            "k"         -> steps._1.settings.numberOfClusters,
            "s1. peak"  -> max(steps._1.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
            "s1. agg m" -> steps._1.aggregatedMetric,
            "s1. max m" -> steps._1.clusters.map(steps._2.settings.metric(_)).max,
            "s2. peak"  -> max(steps._2.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
            "s2. agg m" -> steps._2.aggregatedMetric,
            "s2. max m" -> steps._2.clusters.map(steps._2.settings.metric(_)).max,
            "total m" -> Point
              .pointListToVector(steps._2.clusters.flatMap(_.points))
              .map(vec => steps._2.settings.metric(vec)),
            "clusters" -> steps._1.clusters.map(_.points.size)
          )
      }

      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }


  }

}

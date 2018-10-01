package algorithm.algorithms.euclidean
import java.io.PrintWriter

import batch.GenBatchRun
import breeze.linalg.max
import config.Configuration
import eventmanager.EventManager
import algorithm.algorithms.brute.Main.readEgaugeData
import metrics.Par
import play.api.libs.json.Json
import types.{Cluster, Point}
import utils.FileUtils
import algorithm.serialization.EuclideanClustererSettingsJsonSerializer._
import algorithm.serialization.EuclideanAlgorithmJsonSerializer._
import algorithm.serialization.ResultsJsonSerializer
import types.serialization.ClusterJsonSerializer._


object Main {

  def main(args: Array[String]): Unit = {
/*    val points = Generator
      .generateRandom2DPoints(Vector(0.0, 0.0), 5, 50, 5)
      .zipWithIndex
      .map {
        case (m, idx) =>
          Point(idx, m.toDenseVector.asDenseMatrix, None)(Types2)
      }
      .toVector*/

    var clustersBuffer: List[List[Cluster]] = Nil
    EventManager.singleton
      .subscribe("clusters",
                 (topic: String, event: Object) => clustersBuffer = event.asInstanceOf[List[Cluster]] :: clustersBuffer)

     val points =   readEgaugeData("files/input/egauge.json").toVector

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

    // Windows
/*    FileUtils.copyFile("files/output/cluster.json",
                       "/Users/vcaballero/Projects/jupyter-notebook/siot-eclustering-viz/files")

    FileUtils.copyFile(Configuration.summaryBatchRunFile,
                       "/Users/vcaballero/Projects/jupyter-notebook/siot-eclustering-viz/files")
    FileUtils.copyFile(Configuration.batchRunFile,
                       "/Users/vcaballero/Projects/jupyter-notebook/siot-eclustering-viz/files")*/

    // Mac
    FileUtils.copyFile("files/output/cluster.json",
      "/Users/vicaba/Projects/jupyter/shared/siot-eclustering-viz/files")

    FileUtils.copyFile(Configuration.summaryBatchRunFile,
      "/Users/vicaba/Projects/jupyter/shared/siot-eclustering-viz/files")
    FileUtils.copyFile(Configuration.batchRunFile,
      "/Users/vicaba/Projects/jupyter/shared/siot-eclustering-viz/files")

  }

  def batchRun(points: scala.Vector[Point]) = {

    val batchRunnerSettingsBuilder = new BatchRunSettingsBuilder(points,
                                                                 (1 to 3).toList,
                                                                 List(Par.withParAggregate),
                                                                 (points, k) => points.size * k)

    val stepsList = GenBatchRun(EuclideanAlgorithm)(batchRunnerSettingsBuilder.build)

    Some(new PrintWriter(Configuration.batchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.batchRunAsJson(stepsList)

      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.summaryBatchRunAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

  }

}

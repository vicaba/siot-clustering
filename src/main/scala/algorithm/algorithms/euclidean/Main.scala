package algorithm.algorithms.euclidean
import java.io.PrintWriter

import batch.GenBatchRun
import config.Configuration
import eventmanager.EventManager
import algorithm.algorithms.brute.Main.readEgaugeData
import algorithm.clusterer.FlattenedEuclideanClusterer
import metrics.Par
import play.api.libs.json.Json
import types.{Cluster, Point, Types2}
import utils.{FileUtils, Generator}
import algorithm.serialization.EuclideanAlgorithmJsonSerializer._
import algorithm.serialization.{EuclideanAlgorithmJsonSerializer, ResultsJsonSerializer}
import breeze.linalg.DenseVector
import crossfold.CrossFoldValidation
import crossfold.CrossFoldValidation.{MonteCarlo, Percentage}
import types.serialization.ClusterJsonSerializer._

import scala.collection.mutable

object Main {

  def main(args: Array[String]): Unit = {
    /*val points = Generator
      .generateRandom2DPoints(DenseVector(0.0, 0.0), 5, 189, 5)
      .zipWithIndex
      .map {
        case (m, idx) =>
          Point(idx, m.toDenseVector.asDenseMatrix, None)(Types2)
      }
      .toVector*/

    val points = readEgaugeData("files/input/egauge.json")

    val batchRunSettingsBuilder =
      new BatchRunSettingsBuilder(points, (1 to 2).toList, List(Par.withParAggregate), (points, k) => 200)

    crossFoldValidation(batchRunSettingsBuilder)

    val filePath = "w" match {
      case "w" => "/Users/vcaballero/Projects/jupyter-notebook/siot-eclustering-viz/files"
      case "m" => "/Users/vicaba/Projects/jupyter/shared/siot-eclustering-viz/files"
    }

    FileUtils.copyFile("files/output/cluster.json", filePath)

    FileUtils.copyFile(Configuration.summaryBatchRunFile, filePath)

    FileUtils.copyFile(Configuration.batchRunFile, filePath)

  }

  def batchRun(batchRunSettingsBuilder: BatchRunSettingsBuilder): Unit = {

    val stepsList = GenBatchRun(EuclideanAlgorithm)(batchRunSettingsBuilder.build)

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

  def batchRunCluster(batchRunSettingsBuilder: BatchRunSettingsBuilder): Unit = {

    val stepsList = GenBatchRun.cluster(EuclideanAlgorithm)(batchRunSettingsBuilder.build.map(_._1))

    Some(new PrintWriter(Configuration.batchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.clustererBatchRunAsJson(stepsList)

      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.summaryClustererBatchRunAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

  }

  def crossFoldValidation(batchRunSettingsBuilder: BatchRunSettingsBuilder): Unit = {

    val Max         = 10
    val monteCarlos = for (i <- 9 to Max) yield { MonteCarlo(1, Percentage.of(i / Max)) }
    val stepsList   = CrossFoldValidation.batchRun(EuclideanAlgorithm)(monteCarlos.toList, batchRunSettingsBuilder)

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.summaryCrossfoldBatchRunAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

  }

}

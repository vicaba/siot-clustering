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

    val points = readEgaugeData(Configuration.userProfilesFile).take(4)

    val batchRunSettingsBuilder =
      new BatchRunSettingsBuilder(points,
                                  (Configuration.BatchRun.KRange.from to Configuration.BatchRun.KRange.to).toList,
                                  List(Par.withParAggregate),
                                  (points, k) => points.size + points.size/3)

    crossFoldValidation(batchRunSettingsBuilder)

    val filePath = "w" match {
      case "w" => "/Users/vcaballero/Projects/jupyter-notebook/siot-eclustering-viz/files"
      case "m" => "/Users/vicaba/Projects/jupyter/shared/siot-eclustering-viz/files"
    }

/*    FileUtils.copyFile("files/output/cluster.json", filePath)

    FileUtils.copyFile(Configuration.summaryBatchRunFile, filePath)

    FileUtils.copyFile(Configuration.batchRunFile, filePath)*/

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
      val jsonList = ResultsJsonSerializer.summaryStep1ListAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

  }

  def crossFoldValidation(batchRunSettingsBuilder: BatchRunSettingsBuilder): Unit = {

    val Max = BigDecimal(1.0)
    val monteCarlos = for (i <- BigDecimal(Configuration.CrossFold.SubsampleSize.from) to (Max, step = BigDecimal(0.1)))
      yield {
        val subsampleSize = Percentage.of(i / Max)
        val splits = Math.floor((batchRunSettingsBuilder.points.size * subsampleSize.v).toDouble).toInt
        MonteCarlo(splits, subsampleSize)
      }
    val stepsList = CrossFoldValidation.batchRun(EuclideanAlgorithm)(monteCarlos.toList, batchRunSettingsBuilder)

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.summaryCrossfoldBatchRunAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

  }

}

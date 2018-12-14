package algorithm.algorithms.euclidean
import java.io.PrintWriter

import batch.GenBatchRun
import config.Configuration

import metrics.Par
import play.api.libs.json.Json
import reader.Reader._
import types.DataTypeMetadata2Columns
import algorithm.serialization.EuclideanAlgorithmJsonSerializer._
import algorithm.serialization.ResultsJsonSerializer
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

    val points = readEgaugeData(Configuration.userProfilesFile)

    //TODO: Why defaulting to points.size + points.size/3?
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
        val splits = Math.floor((batchRunSettingsBuilder.points.size * subsampleSize.v).toDouble).toInt / 2
        MonteCarlo(1, subsampleSize)
      }
    val stepsList = CrossFoldValidation.batchRunClusterer(EuclideanAlgorithm)(monteCarlos.toList, batchRunSettingsBuilder)

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.summaryCrossfoldBatchRunClustererAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

  }

}

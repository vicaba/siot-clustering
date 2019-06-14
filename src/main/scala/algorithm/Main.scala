package algorithm
import java.io.PrintWriter

import algorithm.serialization.ResultsJsonSerializer
import batch.GenBatchRun
import config.Configuration
import crossfold.CrossFoldValidation
import crossfold.CrossFoldValidation.{MonteCarlo, Percentage}
import metrics.Par
import play.api.libs.json.Json
import algorithm.serialization.AlgorithmJsonSerializer._
import reader.EgaugeReader

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

    val points = EgaugeReader(Configuration.userProfilesFile)

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

    val stepsList = GenBatchRun(batchRunSettingsBuilder.build)

    Some(new PrintWriter(Configuration.batchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.batchRunAsJson(stepsList)

      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.Summary.batchRunAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

  }

  def batchRunCluster(batchRunSettingsBuilder: BatchRunSettingsBuilder): Unit = {

    val stepsList = GenBatchRun.cluster(batchRunSettingsBuilder.build.map(_._1))

    Some(new PrintWriter(Configuration.batchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.clustererOutputBatchRunAsJson(stepsList)

      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.Summary.clustererOutputListAsJson(stepsList)
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
        MonteCarlo(splits, subsampleSize)
      }
    val stepsList = CrossFoldValidation.batchRunClusterer(monteCarlos.toList, batchRunSettingsBuilder)

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.Summary.clustererOutputCrossfoldBatchRunAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

  }

}

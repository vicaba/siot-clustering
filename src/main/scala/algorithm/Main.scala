package algorithm

import java.io.PrintWriter

import algorithm.EuclideanAlgorithm.ClustererAndReschedulerOutput
import algorithm.serialization.ResultsJsonSerializer
import batch.GenBatchRun
import config.{Configuration, EgaugeGlobalConfig, GlobalConfig, SyntheticGlobalConfig}
import crossfold.CrossFoldValidation
import crossfold.CrossFoldValidation.{MonteCarlo, Percentage}
import metrics.Par
import play.api.libs.json.Json
import algorithm.serialization.AlgorithmJsonSerializer._
import reader.{EgaugeReader, SyntheticProfilesReaderForEuclideanClusterer}
import types.clusterer.immutable.Point

import scala.util.Try

object Main {

  def main(args: Array[String]): Unit = {
    println(Runtime.getRuntime.availableProcessors())
    /*val points = Generator
      .generateRandom2DPoints(DenseVector(0.0, 0.0), 5, 189, 5)
      .zipWithIndex
      .map {
        case (m, idx) =>
          Point(idx, m.toDenseVector.asDenseMatrix, None)(Types2)
      }
      .toVector*/

    val MainFolder = "files/syn_loads/"
    val AppliancesOutputFileName = "appliance_output.csv"
    val LightingOutputFileName = "lighting_output.csv"

    val subFoldersAndIds: List[(String, Int)] = (for (i <- 0 to 199) yield (i + "/", i)).toList

    val points = SyntheticProfilesReaderForEuclideanClusterer
      .applyDefault(MainFolder,
        subFoldersAndIds.map(_._1),
        AppliancesOutputFileName,
        LightingOutputFileName,
        subFoldersAndIds.map(_._2),
        windowSize = 30)

    GlobalConfig.instance = SyntheticGlobalConfig()

    /*val points = EgaugeReader(Configuration.userProfilesFile).take(10)

    GlobalConfig.instance = EgaugeGlobalConfig()*/

    /*    val testBatchRunSettingsBuilder =
          new BatchRunSettingsBuilder(points,
          List(4),
          List(Par.withParAggregate),
          (_, _) => 1)

        batchRunCluster(testBatchRunSettingsBuilder)*/

    val batchRunSettingsBuilder =
      new BatchRunSettingsBuilder(points,
        (Configuration.BatchRun.KRange.from to Configuration.BatchRun.KRange.to).toList,
        List(Par.withParAggregate),
        (p, k) => 1)

    deferredCrossFoldValidation(batchRunSettingsBuilder)

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

/*  def crossFoldValidation(batchRunSettingsBuilder: BatchRunSettingsBuilder): Unit = {

    val Max = BigDecimal(1.0)
    val monteCarlos = //List(MonteCarlo(1, Percentage.of(Max)))
      for (i <- BigDecimal(Configuration.CrossFold.SubsampleSize.from) to(Max, step = BigDecimal(0.1)))
        yield {
          val subsampleSize = Percentage.of(i / Max)
          val splits = 200 //Math.floor((batchRunSettingsBuilder.points.size * subsampleSize.v).toDouble).toInt / 2
          MonteCarlo(splits, subsampleSize)
        }
    val stepsList = CrossFoldValidation.batchRun(monteCarlos.toList, batchRunSettingsBuilder)

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = ResultsJsonSerializer.Summary.crossfoldBatchRunAsJson(stepsList)
      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }
  }*/

  def deferredCrossFoldValidation(batchRunSettingsBuilder: BatchRunSettingsBuilder): Unit = {

    val Max = BigDecimal(1.0)
    val monteCarlos = //List(MonteCarlo(1, Percentage.of(Max)))
      for (i <- BigDecimal(Configuration.CrossFold.SubsampleSize.from) to(Max, step = BigDecimal(0.1)))
        yield {
          val subsampleSize = Percentage.of(i / Max)
          val splits = 5000 //Math.floor((batchRunSettingsBuilder.points.size * subsampleSize.v).toDouble).toInt / 2
          MonteCarlo(splits, subsampleSize)
        }
    val stepsList = CrossFoldValidation.deferredBatchRun(monteCarlos.toList, batchRunSettingsBuilder)

    stepsList.foreach { crossFoldValidationExperiment =>
      val crossFoldValidationExperimentSettings = crossFoldValidationExperiment._1
      val deferredExperiments = crossFoldValidationExperiment._2
      deferredExperiments.grouped(500).zipWithIndex.foreach { case (groupForDeferredExperiment, groupIteration) =>
        val groupForResolvedExperiment = groupForDeferredExperiment.map(_.map(_.apply()))

        val mockedStepsList: List[(CrossFoldValidation.CrossFoldTypeSettings, List[List[ClustererAndReschedulerOutput]])] =
          List((crossFoldValidationExperimentSettings, groupForResolvedExperiment))

        val subSampleSize = crossFoldValidationExperimentSettings.subSampleSize.v

        val filename =
          s"${Configuration.summaryBatchRunFile.split('.').head}_ss-${subSampleSize}_git-${groupIteration}.json"

        Some(new PrintWriter(filename)).foreach { p =>
          println("FILE WRITE")
          val jsonList = ResultsJsonSerializer.Summary.crossfoldBatchRunAsJson(mockedStepsList)
          p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
          p.close()
        }
        println("FILE WRITE")
      }
    }
  }

  def crossFoldValidationClusterer(batchRunSettingsBuilder: BatchRunSettingsBuilder): Unit = {

    val Max = BigDecimal(1.0)
    val monteCarlos = for (i <- BigDecimal(Configuration.CrossFold.SubsampleSize.from) to(Max, step = BigDecimal(0.1)))
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

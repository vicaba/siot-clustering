package main


import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}

import algorithm.Algorithm
import breeze.linalg.{DenseMatrix, DenseVector}
import algorithm.clusterer.Clusterer
import algorithm.serialization.AlgorithmJsonSerializer._
import algorithm.scheduler.ClusterRescheduler
import batch.BatchRun
import batch.BatchRun.BatchRunSettingsBuilder
import types._
import types.Types._
import config.Configuration
import metrics.Metric
import play.api.libs.json.Json
import reader.Reader
import types.Point
import types.serialization.ClusterJsonSerializer._

object Main {

  def copyFile(srcFile: String, dstFile: String): Unit = {
    val src = new File(srcFile)
    val _dest = new File(dstFile)
    val dest =
      if (_dest.isFile) _dest
      else new File(_dest.getAbsolutePath + "/" + src.getName)

    println(dest.getAbsolutePath)

    new FileOutputStream(dest) getChannel() transferFrom(
      new FileInputStream(src) getChannel(), 0, Long.MaxValue)
  }

  def main(args: Array[String]): Unit = {
/*    val points = Reader.readUserRanges(Configuration.userProfilesFile).zipWithIndex.map { case (values, idx) =>
      implicit val types: TypesT = Types24
      val v = EmptyData()
      v(0, ::) := DenseVector[Double](values: _*).t
      Point(idx, v)
    }*/

    val points = List(
      DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
      , DenseMatrix((5.0, 0.0 , 5.0, 0.0), (5.0, 0.0, 5.0, 0.0))
      , DenseMatrix((3.0, 0.0 , 0.0, 3.0), (4.0, 0.0, 0.0, 4.0))
      , DenseMatrix((0.0, 5.0, 0.0, 5.0), (0.0, 5.0, 0.0, 5.0))
      , DenseMatrix((1.0, 5.0, 5.0, 5.0), (0.0, 2.0, 3.0, 5.0))
      , DenseMatrix((8.0, 1.0, 0.0, 0.0), (0.0, 1.0, 0.0, 1.0))
      , DenseMatrix((1.0, 0.0, 2.0, 0.0))
      , DenseMatrix((4.0, 1.0, 3.0, 7.0))
      , DenseMatrix((10.0, 10.0, 10.0, 10.0), (1.0, 1.0, 1.0, 1.0), (0.0, 17.0, 1.0, 6.0))
      , DenseMatrix((0.0, 12.0, 12.0, 12.0))
    ).zipWithIndex.map { case (m, idx) =>
      Point(idx, m, None)(Types4)
    }.toVector

    val batchRunnerSettingsBuilder = new BatchRunSettingsBuilder(points, (5 to 5).toList, List(Metric.par), (points, k) => points.size * 100 * k)

    val stepsList = BatchRun(batchRunnerSettingsBuilder).zipWithIndex

    Some(new PrintWriter(Configuration.batchRunFile)).foreach { p =>
      val jsonList = stepsList.map { case (steps, idx) =>
        Json.obj(
          "run" -> idx,
          "steps" -> Json.toJson(steps)
        )
      }

      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

    /*    val runSettings = Clusterer.Settings(numberOfClusters = 2, points.take(30), Metric.par, times = points.take(30).size * 100)

        val reschedulerSettings = ClusterRescheduler.Settings(Metric.par, 0.5, memory = 2)

        val steps = Algorithm(runSettings, reschedulerSettings)

        Some(new PrintWriter(Configuration.clustererFile)).foreach{ p =>
          p.write(Json.toJson(steps._1.clusters).toString()); p.close
        }

        Some(new PrintWriter(Configuration.reschedulerFile)).foreach{ p =>
          p.write(Json.toJson(steps._2.clusters).toString()); p.close
        }

        copyFile(Configuration.clustererFile, "/home/vcaballero/Projects/jupyter-datascience.d/files/")
        copyFile(Configuration.reschedulerFile, "/home/vcaballero/Projects/jupyter-datascience.d/files/")*/

  }

}

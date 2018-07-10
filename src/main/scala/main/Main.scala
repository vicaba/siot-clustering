package main


import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}

import breeze.linalg.{DenseMatrix, DenseVector}
import algorithm.{Algorithm, Clusterer}
import algorithm.Clusterer.Settings
import algorithm.scheduler.ClusterRescheduler
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
    val points = Reader.readUserRanges(Configuration.userProfilesFile).zipWithIndex.map { case (values, idx) =>
      implicit val types: TypesT = Types25
      val v = EmptyData()
      v(0, ::) := DenseVector[Double](values: _*).t
      Point(idx, v)
    }

    /*val points = List(
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
    }.toVector*/

    val runSettings = Clusterer.Settings(numberOfClusters = 2, points, Metric.par, times = points.size * 100)

    val reschedulerSettings = ClusterRescheduler.Settings(Metric.par, 0.5, memory = 2)

    val steps = Algorithm.apply(runSettings, reschedulerSettings)

    Some(new PrintWriter(Configuration.clustererFile)).foreach{ p =>
      p.write(Json.toJson(steps(0).clusters).toString()); p.close
    }

    Some(new PrintWriter(Configuration.reschedulerFile)).foreach{ p =>
      p.write(Json.toJson(steps(1).clusters).toString()); p.close
    }

    copyFile(Configuration.clustererFile, "/home/vcaballero/Projects/jupyter-datascience.d/files/")
    copyFile(Configuration.reschedulerFile, "/home/vcaballero/Projects/jupyter-datascience.d/files/")

  }

}

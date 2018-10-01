package algorithm.algorithms.brute

import java.io._
import java.nio.file.Paths

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{FileIO, Keep, Sink, Source}
import akka.util.ByteString
import algorithm.algorithms.GenAlgorithm
import algorithm.algorithms.brute.{BatchRunSettingsBuilder, BruteAlgorithm}
import breeze.linalg.{DenseMatrix, DenseVector, max}
import algorithm.clusterer.BruteClusterer
import algorithm.serialization.BruteAlgorithmJsonSerializer._
import batch.GenBatchRun
import types._
import config.Configuration
import eventmanager.Subscriber
import metrics.Par
import play.api.libs.json.{JsObject, JsValue, Json, OWrites}
import types.Point
import types.serialization.ClusterJsonSerializer._
import algorithm.serialization.BruteClustererSettingsJsonSerializer._
import algorithm.serialization.ResultsJsonSerializer
import utils.FileUtils

import scala.util.Try

object Main {

  def copyFile(srcFile: String, dstFile: String): Unit = FileUtils.copyFile(srcFile, dstFile)

  def readEgaugeData(file: String): Vector[Point] = {

    val stream = new FileInputStream(file)
    Try(Json.parse(stream)).fold(
      _ => {
        stream.close()
        Vector.empty[Point]
      },
      jsval => {
        stream.close()
        jsval
          .validate[List[JsValue]]
          .fold(
            _ => Vector.empty[Point],
            jsList => {
              jsList.map { jsPoint =>
                val dataid = (jsPoint \ "dataid").validate[Int].get
                val vectorList = (jsPoint \ "data").validate[List[List[Double]]].get.map { applianceList =>
                  val vector = DenseVector(applianceList: _*)
                  if (vector.length == 0) {
                    vector
                  } else {
                    vector
                  }
                }
                val data = DenseMatrix(vectorList: _*)

                val point = Point(dataid, data, None)(Types67_24)

                point
              }.toVector
            }
          )
      }
    )

  }

  def main(args: Array[String]): Unit = {
    /*    val points = Reader.readUserRanges(Configuration.userProfilesFile).zipWithIndex.map { case (values, idx) =>
          implicit val types: TypesT = Types24
          val v = EmptyData()
          v(0, ::) := DenseVector[Double](values: _*).t
          Point(idx, v)
        }*/

    /*val points = List(
      DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
      , DenseMatrix((10.0, 10.0, 10.0, 10.0), (1.0, 1.0, 1.0, 1.0), (0.0, 17.0, 1.0, 6.0))
      , DenseMatrix((5.0, 0.0, 5.0, 0.0), (5.0, 0.0, 5.0, 0.0))
      , DenseMatrix((1.0, 5.0, 5.0, 5.0), (0.0, 2.0, 3.0, 5.0))
      , DenseMatrix((3.0, 0.0, 0.0, 3.0), (4.0, 0.0, 0.0, 4.0))
      , DenseMatrix((0.0, 5.0, 0.0, 5.0), (0.0, 5.0, 0.0, 5.0))
      , DenseMatrix((8.0, 1.0, 0.0, 0.0), (0.0, 1.0, 0.0, 1.0))
      , DenseMatrix((1.0, 0.0, 2.0, 0.0))
      , DenseMatrix((4.0, 1.0, 3.0, 7.0))
      , DenseMatrix((0.0, 12.0, 12.0, 12.0))
    ).zipWithIndex.map { case (m, idx) =>
      Point(idx, m, None)(Types4)
    }.toVector*/

    val points = readEgaugeData("files/input/egauge.json")

    batchRun(points)

  }

  def batchRun(points: scala.Vector[Point]) = {

    val batchRunnerSettingsBuilder = new BatchRunSettingsBuilder(points,
                                                                 (1 to 6).toList,
                                                                 List(Par.withAverageAggregate),
                                                                 (points, k) => points.size * k)

    val stepsList = GenBatchRun(BruteAlgorithm)(batchRunnerSettingsBuilder.build)

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

    //copyFile(Configuration.summaryBatchRunFile, "/Users/vcaballero/Projects/jupyter-notebook/siot-clustering-viz/")
    //copyFile(Configuration.batchRunFile, "/Users/vcaballero/Projects/jupyter-notebook/siot-clustering-viz/")

  }

}

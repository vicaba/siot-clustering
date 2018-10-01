package main

import java.io._
import java.nio.file.Paths

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{FileIO, Keep, Sink, Source}
import akka.util.ByteString
import breeze.linalg.{DenseMatrix, DenseVector, max}
import algorithm.clusterer.BruteClusterer
import algorithm.serialization.BruteAlgorithmJsonSerializer._
import batch.BatchRun
import batch.BatchRun.BatchRunSettingsBuilder
import types._
import config.Configuration
import eventmanager.{EventManager, Subscriber}
import metrics.Par
import play.api.libs.json.{JsObject, JsValue, Json}
import types.Point
import types.serialization.ClusterJsonSerializer._
import algorithm.serialization.BruteClustererSettingsJsonSerializer._
import com.typesafe.scalalogging.Logger
import util.FileUtils

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

  case class IterationLogger(actorSystem: ActorSystem, fileName: String) extends Subscriber {

    val bufferSize       = 100
    val overflowStrategy = akka.stream.OverflowStrategy.dropHead

    implicit val materializer = ActorMaterializer()(actorSystem)

    val queue = Source
      .queue[(BruteClusterer.Settings, List[Cluster])](bufferSize, overflowStrategy)
      .map(tupleToJson)
      .map(Json.prettyPrint)
      .map(json => ByteString(json + ", "))
      .toMat(FileIO.toPath(Paths.get(fileName)))(Keep.left)
      .run()

    override def onEvent(topic: String, event: Object): Unit = topic match {
      case "iteration" =>
        val iteration = event.asInstanceOf[(BruteClusterer.Settings, List[Cluster])]
        queue offer iteration
      case _ =>
    }

    private def tupleToJson(iteration: (BruteClusterer.Settings, List[Cluster])): JsObject = {
      val oppositeMetric =
        if (iteration._1.metric == Par.withAverageAggregate) Par.withParAggregate
        else Par.withAverageAggregate
      Json.obj(
        "settings"             -> iteration._1,
        "clusters"             -> iteration._2,
        "clusterPoints"        -> iteration._2.map(_.points.size),
        "aggregatedMetric"     -> iteration._1.metric.aggregateOf(iteration._2),
        "aggregateMetricName"  -> iteration._1.metric.aggregateOf.toString,
        "aggregatedMetric2"    -> oppositeMetric.aggregateOf(iteration._2),
        "aggregateMetric2Name" -> oppositeMetric.toString
      )
    }

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

    //copyFile(Configuration.summaryBatchRunFile, "/Users/vcaballero/Projects/jupyter-notebook/siot-clustering-viz/")
    //copyFile(Configuration.batchRunFile, "/Users/vcaballero/Projects/jupyter-notebook/siot-clustering-viz/")

  }

  def iterationRun(points: scala.Vector[Point]): Unit = {

    // Create subscribers
    val iterationLogger = IterationLogger(ActorSystem(), Configuration.clustererFile)
    EventManager.singleton.subscribe("iteration", iterationLogger)

    val batchRunnerSettingsBuilder =
      new BatchRunSettingsBuilder(points,
                                  (1 to 5).toList,
                                  List(Par.withParAggregate, Par.withAverageAggregate),
                                  (points, k) => points.size * k)

    val logger = Logger("iteration")

    BatchRun(
      batchRunnerSettingsBuilder, { clustererSettings =>
        logger.info("clusterer settings: {}", clustererSettings)
        val r = BruteClusterer(clustererSettings)
        logger.info("done")
        r
      }
    )

  }

}

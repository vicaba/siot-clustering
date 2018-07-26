package main


import java.io._

import algorithm.Algorithm
import breeze.linalg.{DenseMatrix, DenseVector, max}
import algorithm.clusterer.Clusterer
import algorithm.serialization.AlgorithmJsonSerializer._
import algorithm.scheduler.ClusterRescheduler
import batch.BatchRun
import batch.BatchRun.BatchRunSettingsBuilder
import types._
import types.Types._
import config.Configuration
import eventmanager.{EventManager, Subscriber}
import metrics.{Metric, Par}
import org.apache.commons.csv.CSVFormat
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import reader.Reader
import types.Point
import types.serialization.ClusterJsonSerializer._
import algorithm.serialization.ClustererJsonSerializer._
import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

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

  def readEgaugeData(file: String): Vector[Point] = {

    val stream = new FileInputStream(file)
    Try(Json.parse(stream)).fold(_ => {
      stream.close()
      Vector.empty[Point]
    },
      jsval => {
        stream.close()
        jsval.validate[List[JsValue]].fold(_ => Vector.empty[Point], jsList => {
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
        })
      })

  }

  case class IterationLogger() extends Subscriber {

    val map = new mutable.HashMap[Int, ListBuffer[(Clusterer.Settings, List[Cluster])]]()

    override def onEvent(topic: String, event: Object): Unit = topic match {
      case "iteration" =>
        val iteration = event.asInstanceOf[(Clusterer.Settings, List[Cluster])]
        val iterationList = map.applyOrElse(iteration._1.numberOfClusters, { _: Int => ListBuffer[(Clusterer.Settings, List[Cluster])]() })
        iterationList.+=(iteration)
        map.+=(iteration._1.numberOfClusters -> iterationList)
      case _ =>
    }

    def toJson: JsValue = JsArray(map.map { it =>
      Json.obj(
        it._1.toString -> it._2.map { itList =>
          val oppositeMetric =
            if (itList._1.metric == Par.withAverageAggregate) Par.withParAggregate
            else Par.withAverageAggregate
          Json.obj(
            "settings" -> itList._1,
            "clusters" -> itList._2,
            "clusterPoints" -> itList._2.map(_.points.size),
            "aggregatedMetric" -> itList._1.metric.aggregateOf(itList._2),
            "aggregateMetricName" -> itList._1.metric.aggregateOf.toString,
            "aggregatedMetric2" -> oppositeMetric.aggregateOf(itList._2),
            "aggregateMetric2Name" -> oppositeMetric.toString
          )
        }.toList
      )
    }.toList)

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

    iterationRun(points)

  }

  def batchRun(points: scala.Vector[Point]) = {

    // Create subscribers
    val iterationLogger = IterationLogger()
    EventManager.singleton.subscribe("iteration", iterationLogger)

    val batchRunnerSettingsBuilder = new BatchRunSettingsBuilder(points, (1 to 5).toList, List(Metric.par), (points, k) => points.size * k)

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

    Some(new PrintWriter(Configuration.summaryBatchRunFile)).foreach { p =>
      val jsonList = stepsList.map { case (steps, idx) =>
        Json.obj(
          "k" -> steps._1.settings.numberOfClusters,
          "s1. peak" -> max(steps._1.clusters.maxBy(c => max(c.syntheticCenter)).syntheticCenter),
          "s1. agg m" -> steps._1.aggregatedMetric,
          "s1. max m" -> steps._1.clusters.map(steps._2.settings.metric(_)).max,
          "s2. peak" -> max(steps._2.clusters.maxBy(c => max(c.syntheticCenter)).syntheticCenter),
          "s2. agg m" -> steps._2.aggregatedMetric,
          "s2. max m" -> steps._2.clusters.map(steps._2.settings.metric(_)).max,
          "total m" -> Point.pointListToVector(steps._2.clusters.flatMap(_.points)).map(vec => steps._2.settings.metric(vec)),
          "clusters" -> steps._1.clusters.map(_.points.size)
        )
      }


      p.write(Json.prettyPrint(Json.toJson(jsonList)).toString())
      p.close()
    }

    copyFile(Configuration.summaryBatchRunFile, "/Users/vicaba/Projects/jupyter/shared/siot-clustering-viz/")
    copyFile(Configuration.batchRunFile, "/Users/vicaba/Projects/jupyter/shared/siot-clustering-viz/")

  }

  def iterationRun(points: scala.Vector[Point]): Unit = {

    // Create subscribers
    val iterationLogger = IterationLogger()
    EventManager.singleton.subscribe("iteration", iterationLogger)

    val batchRunnerSettingsBuilder = new BatchRunSettingsBuilder(points, (1 to 5).toList, List(Par.withParAggregate, Par.withAverageAggregate), (points, k) => points.size * k)

    val logger = Logger("iteration")

    BatchRun(batchRunnerSettingsBuilder, { clustererSettings =>
      logger.info("clusterer settings: {}", clustererSettings)
      val r = Clusterer(clustererSettings)
      logger.info("done")
      r
    })

    Some(new PrintWriter(Configuration.clustererFile)).foreach { p =>
      p.write(Json.prettyPrint(iterationLogger.toJson))
      p.close()
    }

  }

}

package releases

import java.io.PrintWriter

import algorithm.clusterer.{EuclideanClusterer, EuclideanClustererSettings}
import algorithm.serialization.ResultsJsonSerializer
import breeze.linalg.DenseMatrix
import collection.shuffler.Keep
import config.Configuration
import metrics.Par
import play.api.libs.json.Json
import types.clusterer.{DataTypeMetadata, DataTypeMetadata4Columns}
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster

object Releases {

  val points = List(
    DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0)),
    DenseMatrix((5.0, 0.0, 5.0, 0.0), (5.0, 0.0, 5.0, 0.0)),
    DenseMatrix((3.0, 0.0, 0.0, 3.0), (4.0, 0.0, 0.0, 4.0)),
    DenseMatrix((0.0, 5.0, 0.0, 5.0), (0.0, 5.0, 0.0, 5.0)),
    DenseMatrix((1.0, 5.0, 5.0, 5.0), (0.0, 2.0, 3.0, 5.0)),
    DenseMatrix((8.0, 1.0, 0.0, 0.0), (0.0, 1.0, 0.0, 1.0)),
    DenseMatrix((1.0, 0.0, 2.0, 0.0)),
    DenseMatrix((4.0, 3.0, 1.0, 7.0)),
    DenseMatrix((10.0, 10.0, 10.0, 10.0), (1.0, 1.0, 1.0, 1.0), (17.0, 0.0, 1.0, 6.0)),
    DenseMatrix((0.0, 12.0, 12.0, 12.0))
  ).zipWithIndex.map {
    case (m, idx) =>
      Point(idx, m, Nil, None)(DataTypeMetadata4Columns)
  }.toVector

  def serializeResult(result: List[Cluster]): String = Cluster.flattenAsList(result).map(_.id).mkString(",")

  def main(args: Array[String]): Unit = {
    writeBaseRelease()
  }

  def writeBaseRelease(): Unit = {

    val metric: Par = Par.withParAggregate

    val runSettings = EuclideanClustererSettings(3, points, metric, improveIterations = 1, Keep)

    val result  = EuclideanClusterer(runSettings)

    Some(new PrintWriter(Configuration.releaseBaseFile)).foreach { p =>
      p.write(serializeResult(result))
      p.close()
    }

  }

}

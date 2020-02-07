import algorithm.clusterer.{EuclideanClusterer, EuclideanClustererSettings}
import collection.shuffler.{Keep, Random}
import metrics.Par
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import types.clusterer.{DataTypeMetadata, DataTypeMetadata4Columns}
import types.clusterer.immutable.Point

import scala.io.{BufferedSource, Source}

class Releases extends FlatSpec {

  implicit val types: DataTypeMetadata = DataTypeMetadata4Columns

  val points: Vector[Point] = releases.Releases.points
  val metric: Par = Par.withParAggregate

  val runSettings = EuclideanClustererSettings(3, points, metric, improveIterations = 1, Keep)

  val result  = EuclideanClusterer(runSettings)

  val source: BufferedSource = Source.fromFile(config.Configuration.releaseBaseFile)
  val line: String = source.getLines.toList.head
  source.close()

  releases.Releases.serializeResult(result) shouldBe line

}

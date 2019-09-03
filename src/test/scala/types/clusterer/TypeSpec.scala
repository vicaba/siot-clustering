package types.clusterer

import algorithm.clusterer.{EuclideanClusterer, EuclideanClustererSettings}
import breeze.linalg.{DenseMatrix, sum}
import metrics.Par
import org.scalatest.FlatSpec
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster

class TypeSpec extends FlatSpec {

  val points = List(
    DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0)),
    DenseMatrix((5.0, 0.0, 5.0, 0.0), (5.0, 0.0, 5.0, 0.0)),
    DenseMatrix((3.0, 0.0, 0.0, 3.0), (4.0, 0.0, 0.0, 4.0))
  ).zipWithIndex.map {
    case (m, idx) =>
      Point(idx, m, Nil, None)(DataTypeMetadata.generateDataTypeMetadata(4))
  }.toVector

  val runSettings = EuclideanClustererSettings(1, points, Par.withAverageAggregate, improveIterations = 100)
  val result      = EuclideanClusterer(runSettings)

  assert(sum(result.head.centroid) == sum(Type.centroidOf(List(result.head))))

  val centroid0 = sum(result.head.centroid)
  val p = result.head.points.toList(0)
  val resultMinus = result.head.-=(p)
  val centroid1 = sum(Type.centroidOf(List(resultMinus, p)))

  assert(centroid0 == centroid1)


}

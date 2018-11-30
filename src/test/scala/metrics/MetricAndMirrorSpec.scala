package metrics

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import types.{DataTypeMetadata, DataTypeMetadata2Columns}
import algorithm.clusterer.FlattenedEuclideanClusterer._
import org.scalatest.Matchers._
import types.immutable.Point
import types.ops.MirrorImage
import types.ops.MirrorImage._

class MetricAndMirrorSpec extends FeatureSpec with GivenWhenThen {

  def vec(values: Double*): DenseVector[Double] = DenseVector[Double](values: _*)

  val vecA = vec(5.0, 0.0)

  val vecB = vec(0.0, 5.0)

  val vecC = vec(5.0, 5.0)

  import MirrorImage._

  val centroid = (vecA + vecB + vecC) / 3.0

  val idealMirror = MirrorImage.findClosestMirror(vecA, centroid, IndexedSeq(vecB, vecC)).get

  val mA1 = MirrorImage.findClosestMirrors(vecA, (vecA + vecB) / 2.0, IndexedSeq(vecB)).head._1

  val mA2 = Metric.par(vecA + vecB)

  val mB1 = MirrorImage.findClosestMirrors(vecA, (vecA + vecC) / 2.0, IndexedSeq(vecC)).head._1

  val mB2 = Metric.par(vecA + vecC)

  info(s"mA1: $mA1, mB1: $mB1")
  assert(mA1 < mB1)

  info(s"mA2: $mA1, mB2: $mB2")
  assert(mA2 < mB2)


}

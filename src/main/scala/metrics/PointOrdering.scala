package metrics

import breeze.numerics.abs
import types.clusterer.immutable.Point
object PointOrdering extends Ordering[Point] {

  override def compare(x: Point, y: Point): Int =
    x.syntheticValue
      .toScalaVector()
      .zip(y.syntheticValue.toScalaVector())
      .map { case (x, y) => abs(x - y) }
      .foldLeft(0) { case (x, y) => (x + y).toInt }

}

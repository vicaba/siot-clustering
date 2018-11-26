package metrics

import breeze.linalg.DenseMatrix
import org.scalatest.{FeatureSpec, GivenWhenThen}
import types.DataTypeMetadata4Columns
import types.immutable.Point

class PointOrderingSpec extends FeatureSpec with GivenWhenThen {

  feature("PointOrdering orders points by compatibility") {
    scenario("Point ordering orders points by compatibility") {
      Given("some points")
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
          Point(idx, m, None)(DataTypeMetadata4Columns)
      }.toVector
      When("asked to order")
      points.sorted(PointOrdering)
      Then("The point with less compatibility compared to the first one is at the end")
    }
  }

}

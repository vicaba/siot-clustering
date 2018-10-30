package algorithm2

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import types.{Cluster, Point, Types2, TypesT}
import algorithm.clusterer.FlattenedEuclideanClusterer._
import org.scalatest.Matchers._
import types.ops.MirrorImage._

class FlattenedEuclideanClustererSpec extends FeatureSpec with GivenWhenThen {

  implicit val types: TypesT = Types2

  def createPoint(v: Vector[Double]): Point = {
    Point(0, DenseMatrix(v), None)
  }

  feature("algorithm2.Ops.findMirror") {
    scenario("Given a point finds the mirror image of it") {

      Given("A point")
      val p = createPoint(Vector(5.0, 7.0))

      And("A center")
      val c = createPoint(Vector(7.0, 10.0))

      When("finding the mirror image")
      val result = findMirror(p, c)

      Then("the result is the expected")
      result shouldBe DenseVector(9.0, 13.0)
    }

    scenario("Given a point finds the closest mirror image from a set of points") {

      val pointsWithoutExactMirror = List(
        DenseMatrix((5.0, 7.0)),
        DenseMatrix((7.0, 10.0)),
        DenseMatrix((10.0, 13.0)),
      ).zipWithIndex.map {
        case (m, idx) =>
          Point(idx, m, None)(Types2)
      }.toVector

      Given("A point")
      val p = pointsWithoutExactMirror(0)

      And("A center")
      val c = pointsWithoutExactMirror(1)

      When("finding the closest mirror image")
      val result = findClosestMirror(p, c, pointsWithoutExactMirror)

      Then("the result is the expected")
      result.get shouldBe pointsWithoutExactMirror(2)
    }

  }
}

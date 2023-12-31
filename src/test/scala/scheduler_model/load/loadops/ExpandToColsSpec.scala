package scheduler_model.load.loadops

import breeze.linalg
import breeze.linalg.DenseVector
import scheduler_model.load.LoadOps
import org.scalatest.{FlatSpec, GivenWhenThen}

class ExpandToColsSpec extends FlatSpec with GivenWhenThen {

  def provider(maxColsToExpand: Int): List[(DenseVector[Double], Int, Int)] = {
    val vector = DenseVector[Double](0.0, 0.0, 2.0)
    val startPositions = for (startPos <- 0 until (maxColsToExpand - vector.length)) yield {
      startPos
    }
    (Stream.fill(startPositions.length)(vector), Stream.fill(startPositions.length)(maxColsToExpand), startPositions).zipped.toList
  }

  def test(vector: DenseVector[Double], colsToExpand: Int, startPosition: Int) = {

    Given(s"a DenseVector $vector")

    When(s"expanded to $colsToExpand columns from position $startPosition")

    val expandedVector: linalg.Vector[Double] = LoadOps.expandToCols(startPosition = startPosition, vector = vector, cols = colsToExpand)

    Then("the expanded slots should have a value of 0.0")

    val expectedExpandedVector = ((for (_ <- 0 until startPosition) yield 0.0) ++
      vector.toScalaVector() ++
      (for (_ <- (startPosition + vector.length) until colsToExpand) yield 0.0)
      ).toVector

    info(s"ExpandedDenseVector: $expandedVector")

    assert(expectedExpandedVector == expandedVector.toDenseVector.toScalaVector())


  }

  "A DenseVector" should "be correctly expanded" in {
    provider(maxColsToExpand = 10).foreach(p => test(p._1, p._2, p._3))
  }

}

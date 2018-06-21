package clustering.scheduler

import breeze.linalg._
import metrics.Metric

object Rescheduler {

  class ChangedComponent(val from: Int, val to: Int)
  class VectorResult[T](val vector: DenseVector[T], val changedComponent: ChangedComponent, val distanceBeforeReschedule: Double, val distanceAfterReschedule: Double)
  class MatrixResult[T](val matrix: DenseMatrix[T], val row: Int, val changedComponent: ChangedComponent, val distanceBeforeReschedule: Double, val distanceAfterReschedule: Double)

  object Mutable {
    def swap(fromIndex: Int, toIndex: Int, vector: DenseVector[Double]): DenseVector[Double] =
    {
      val hold = vector(fromIndex)

      vector(fromIndex) = vector(toIndex)
      vector(toIndex) = hold
      vector
    }
  }

  def swap(fromIndex: Int, toIndex: Int, vector: DenseVector[Double]): DenseVector[Double] =
    Mutable.swap(fromIndex, toIndex, vector.copy)

  /**
   * No side effects
   * @param vectorToReschedule
   * @param fixedVector
   * @return
   */
  def reschedule(vectorToReschedule: DenseVector[Double], fixedVector: DenseVector[Double], metric: Metric): VectorResult[Double] = {

    assert(vectorToReschedule.length == fixedVector.length, "vector lengths are not equal")

    val biggest = metric(vectorToReschedule + fixedVector)
    var smallest = biggest
    var bestSolution = vectorToReschedule
    var changedComponent = null.asInstanceOf[ChangedComponent]
    val length = vectorToReschedule.length
    var i = 0
    var j = 0
    while (i < length) {
      j = i + 1
      while (j < length) {
        val vector = swap(i, j, vectorToReschedule)
        val distance = metric(vector + fixedVector)
        if (distance < smallest) {
          smallest = distance
          bestSolution = vector
          changedComponent = new ChangedComponent(i, j)
        } else {
          vector
        }
        j = j + 1
      }
      i = i + 1
    }

    new VectorResult(bestSolution, changedComponent, biggest, smallest)

  }

  /**
   * No side effects
   * @param matrixToReschedule
   * @param fixedVector
   * @return
   */
  def reschedule(matrixToReschedule: DenseMatrix[Double], fixedVector: DenseVector[Double], metric: Metric): MatrixResult[Double] =
  {

    assert(matrixToReschedule.cols == fixedVector.length, "matrix rows and vector length are not equal")

    /*
    Compute metric for each row and take the first smallest one
     */

    val rowIterator = matrixToReschedule(*, ::).iterator

    var smallest = 0.0
    var bestSolutionRow = 0
    var bestSolution = null.asInstanceOf[VectorResult[Double]]
    var first = true


    rowIterator.zipWithIndex.foreach { case (vector, rowNumber) =>
      val distance = reschedule(vector, fixedVector, metric)

      if (first || distance.distanceAfterReschedule < smallest) {
        first = false

        bestSolutionRow = rowNumber
        smallest = distance.distanceAfterReschedule
        bestSolution = distance
      }
    }

    val result = matrixToReschedule.copy

    result(bestSolutionRow, ::) := bestSolution.vector.t

    new MatrixResult[Double](result, bestSolutionRow, bestSolution.changedComponent, bestSolution.distanceBeforeReschedule, bestSolution.distanceAfterReschedule )

  }

}

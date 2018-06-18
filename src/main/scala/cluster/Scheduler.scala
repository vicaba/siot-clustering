package cluster

import breeze.linalg._
import cluster.Types.{Cluster, Point}
import metrics.Metrics

object Scheduler {

  object Mutable {
    def swap(fromIndex: Int, toIndex: Int, vector: DenseVector[Double]): DenseVector[Double] =
    {
      val hold = vector(fromIndex)

      vector(fromIndex) = vector(toIndex)
      vector(toIndex) = hold
      vector
    }
  }

  class ChangedComponent(val from: Int, val to: Int)
  class VectorResult[T](val vector: DenseVector[T], val changedComponent: ChangedComponent, val distanceBeforeReschedule: Double, val distanceAfterReschedule: Double)
  class MatrixResult[T](val matrix: DenseMatrix[T], val row: Int, val changedComponent: ChangedComponent, val distanceBeforeReschedule: Double, val distanceAfterReschedule: Double)

  def swap(fromIndex: Int, toIndex: Int, vector: DenseVector[Double]): DenseVector[Double] =
    Mutable.swap(fromIndex, toIndex, vector.copy)

  /**
   * No side effects
   * @param vectorToReschedule
   * @param fixedVector
   * @return
   */
  def rescheduleVector(vectorToReschedule: DenseVector[Double], fixedVector: DenseVector[Double]): VectorResult[Double] = {

    assert(vectorToReschedule.length == fixedVector.length, "vector lengths are not equal")

    val biggest = Metrics.par(vectorToReschedule, fixedVector)
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
        val distance = Metrics.par(vector, fixedVector)
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
  def rescheduleMatrix(matrixToReschedule: DenseMatrix[Double], fixedVector: DenseVector[Double]): MatrixResult[Double] =
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
      val distance = rescheduleVector(vector, fixedVector)

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

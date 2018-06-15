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

  class VectorResult[T](val vector: DenseVector[T], val distanceBeforeReschedule: Double, val distanceAfterReschedule: Double)
  class MatrixResult[T](val matrix: DenseVector[T], val row: Int, val column: Int, val distanceBeforeReschedule: Double, val distanceAfterReschedule: Double)

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
        } else {
          vector
        }
        j = j + 1
      }
      i = i + 1
    }

    new VectorResult(bestSolution, biggest, smallest)

  }

  /**
   * No side effects
   * @param matrixToReschedule
   * @param fixedVector
   * @return
   */
  def rescheduleMatrix(matrixToReschedule: DenseMatrix[Double], fixedVector: DenseVector[Double]): DenseMatrix[Double] =
  {

    assert(matrixToReschedule.rows == fixedVector.length, "matrix rows and vector length are not equal")

    /*
    Compute metric for each row and take the first smallest one
     */

    val rowIterator = matrixToReschedule(*, ::).iterator

    var smallest = 0.0
    var bestSolution: DenseVector[Double] = null
    var first = true


    rowIterator.foreach { vector =>
      val distance = rescheduleVector(vector, fixedVector)

      if (first || distance.distanceAfterReschedule < smallest) {
        first = false
        smallest = distance.distanceAfterReschedule
        bestSolution = distance.vector
      }
    }



  }

}

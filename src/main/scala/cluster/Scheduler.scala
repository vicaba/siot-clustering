package cluster

import breeze.linalg.DenseVector
import cluster.Types.{Cluster, Point}
import metrics.Metrics

object Scheduler {

  def rescheduleVector(vectorToReschedule: DenseVector[Int], fixedVector: DenseVector[Int]): DenseVector[Int] = {
    /*
    For every component in the vector
     */
    var smallest = Metrics.par(vectorToReschedule, fixedVector)
    var bestSolution = vectorToReschedule
    val length = vectorToReschedule.length
    var i = 0
    var j = 0
    while (i < length) {
      j = i + 1
      while (j < length) {
        val vector = vectorToReschedule.copy
        vector(i) = vector(j)
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

    bestSolution

  }
}

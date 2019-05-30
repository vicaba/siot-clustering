package test

import test.Load._
import algebra.VectorOps._

object Rescheduler {

  def main(args: Array[String]): Unit = {

    //val fixedLoads = SpanSlotAccumulatedLoad(toFixedLoads(Vector[Double](1, 1, 1, 1, 1, 1, 1, 1))

    val flexibleLoads = Vector(
      SpanSlotFlexibleLoad(1, 0, Vector[Double](1, 1, 1, 1)),
      SpanSlotFlexibleLoad(1, 3, Vector[Double](1, 1, 1, 1))
    )

    //val res1 = findBestContiguousSlots(fixedLoads, flexibleLoads(0))

  }

  def reschedule(accumulatedLoad: SpanSlotAccumulatedLoad,
                 flexibleLoad: SpanSlotFlexibleLoad): SpanSlotAccumulatedLoad = {

    // Used to perform mutable operations
    val temporaryX = accumulatedLoad.copy()

    def localPeakInFlexibleLoadWindow(m: Movement): Double =
      m.acc.amplitudePerSlot.slice(m.fl.positionInT, m.fl.positionInT + m.fl.span).max

    class Movement(val acc: SpanSlotAccumulatedLoad, val fl: SpanSlotFlexibleLoad)

    var first = true

    var bestMovement: Movement = new Movement(accumulatedLoad, flexibleLoad)

    for (i <- accumulatedLoad.positionInT to (accumulatedLoad.span - flexibleLoad.span)) {

      val flexibleLoadMovement = flexibleLoad.copy(positionInT = i)
      val temporaryNewMovement = new Movement(temporaryX -/+= flexibleLoadMovement, flexibleLoadMovement)

      if (first ||
          ((temporaryNewMovement.acc.peak < bestMovement.acc.peak) &&
          (localPeakInFlexibleLoadWindow(temporaryNewMovement) < localPeakInFlexibleLoadWindow(bestMovement)))) {

        first = false
        bestMovement = new Movement(temporaryNewMovement.acc.copy(), temporaryNewMovement.fl)

      }

    }

    bestMovement.acc

  }

  /*  /**
 *
 * @param flexibleLoad
 * @return the index of flexibleLoads where the flexible load start is better suited to reduce PAR
 */
  def findBestContiguousSlots(accumulatedLoad: SpanSlotAccumulatedLoad, flexibleLoad: SpanSlotFlexibleLoad): Int = {
    (for (i <- 0 until (fixedLoads.size - flexibleLoad.span)) yield {
      (i,
       flexibleLoad.amplitudePerSlot
         .zip(fixedLoads.map(_.amplitude).slice(i, flexibleLoad.span + i))
         .map { case (fl, fi) => Math.pow(fl + fi, 2) }
         .sum)
    }).max((x: (Int, Double), y: (Int, Double)) => implicitly[Ordering[Double]].compare(x._2, y._2))._1
  }*/

}

package test

import scala.annotation.tailrec

object Rescheduler {

  def main(args: Array[String]): Unit = {

    //val fixedLoads = SpanSlotAccumulatedLoad(toFixedLoads(Vector[Double](1, 1, 1, 1, 1, 1, 1, 1))

    val flexibleLoads = Vector(
      SpanSlotFlexibleLoad(1, 0, Vector[Double](1, 1, 1, 1)),
      SpanSlotFlexibleLoad(1, 3, Vector[Double](1, 1, 1, 1))
    )

    //val res1 = findBestContiguousSlots(fixedLoads, flexibleLoads(0))

  }

  def reschedule(acc: SpanSlotAccumulatedLoad, preferredSlots: List[Int] = Nil): SpanSlotAccumulatedLoad = {

    @tailrec
    def _reschedule(_acc: SpanSlotAccumulatedLoad,
                    _remainingFlexibleLoads: List[SpanSlotFlexibleLoad]): SpanSlotAccumulatedLoad =
      _remainingFlexibleLoads match {
        case x :: xs => _reschedule(rescheduleFlexibleLoad(_acc, x, preferredSlots), xs)
        case Nil     => _acc
      }

    val remainingLoadsAfterRemovingFlexibleLoads = acc.loads -- acc.flexibleLoads

    if (remainingLoadsAfterRemovingFlexibleLoads.nonEmpty)
      _reschedule(acc.copy(loads = remainingLoadsAfterRemovingFlexibleLoads), acc.flexibleLoads.toList)
    else
      acc.copy()
  }

  def rescheduleFlexibleLoad(accumulatedLoad: SpanSlotAccumulatedLoad,
                             flexibleLoad: SpanSlotFlexibleLoad,
                             preferredSlots: List[Int] = Nil): SpanSlotAccumulatedLoad = {

    // Used to perform mutable operations
    val temporaryX: SpanSlotAccumulatedLoad = accumulatedLoad.copy()

    def incrementInWindow(m: Movement): Double = {
      val slice = m.acc.amplitudePerSlot.slice(m.fl.positionInT, m.fl.positionInT + m.fl.span)
      slice.foldLeft(0.0) {
        case (acc, e) => acc + e * e
      } / slice.size
    }

    var bestMovement: Movement = new Movement(accumulatedLoad += flexibleLoad, flexibleLoad, preferredSlots)
    //println(s"Trying load ${flexibleLoad.id}, load vector = ${flexibleLoad.amplitudePerSlot.toString()}")
    for (i <- accumulatedLoad.positionInT until ((accumulatedLoad.span - flexibleLoad.span) + 1)) {

      val flexibleLoadMovement = flexibleLoad.copy(positionInT = i)
      val temporaryNewMovement =
        new Movement(temporaryX -/+= flexibleLoadMovement, flexibleLoadMovement, preferredSlots)


      //print(s"\tat position $i -> tempPeak = ${temporaryNewMovement.biasedPeak}, bestPeak = ${bestMovement.biasedPeak}")

      if (temporaryNewMovement.acc.peak < bestMovement.acc.peak) {
        //println("\tIs best")

        bestMovement = new Movement(temporaryNewMovement.acc.copy(), temporaryNewMovement.fl, preferredSlots)

      } else {
        //println("\tNot best")
      }

    }

    // TODO: Notice that we are returning a copy of the AccumulatedLoad, with a new mutable Set of Loads.
    bestMovement.acc

  }

}

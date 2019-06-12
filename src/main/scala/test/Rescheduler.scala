package test

import test.RescheduleType.RescheduleType

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

  def reschedule(acc: SpanSlotAccumulatedLoad, preferredSlots: List[Int] = Nil, rescheduleType: RescheduleType, referenceAverage: Double = 0.0, verbose: Boolean = false): SpanSlotAccumulatedLoad = {

    @tailrec
    def _reschedule(_acc: SpanSlotAccumulatedLoad,
                    _remainingFlexibleLoads: List[SpanSlotFlexibleLoad]): SpanSlotAccumulatedLoad =
      _remainingFlexibleLoads match {
        case x :: xs => _reschedule(rescheduleFlexibleLoad(_acc, x, preferredSlots, rescheduleType, referenceAverage, verbose), xs)
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
                             preferredSlots: List[Int] = Nil,
                             rescheduleType: RescheduleType,
                             referenceAverage: Double = 0.0,
                             verbose: Boolean = false): SpanSlotAccumulatedLoad = {

    // Used to perform mutable operations
    val temporaryX: SpanSlotAccumulatedLoad = accumulatedLoad.copy()

    def incrementInWindow(m: Movement): Double = {
      val slice = m.acc.amplitudePerSlot.slice(m.fl.positionInT, m.fl.positionInT + m.fl.span)
      slice.foldLeft(0.0) {
        case (acc, e) => acc + e * e
      } / slice.size
    }

    var bestMovement: Movement = new Movement(accumulatedLoad += flexibleLoad, flexibleLoad, preferredSlots)
    if (verbose) println(s"Trying load ${flexibleLoad.id}, load vector = ${flexibleLoad.amplitudePerSlot.toString()}")
    //if (verbose) println(s"i -> ${accumulatedLoad.positionInT} until ${(accumulatedLoad.span - flexibleLoad.span) + 1}")
    for (i <- accumulatedLoad.positionInT until ((accumulatedLoad.span - flexibleLoad.span) + 1)) {
      if (verbose) println(s"\tAt position $i")

      val flexibleLoadMovement = flexibleLoad.copy(positionInT = i)
      val temporaryNewMovement =
        new Movement(temporaryX -/+= flexibleLoadMovement, flexibleLoadMovement, preferredSlots)

      val (temporaryMetric, bestMetric) = computeMetrics(rescheduleType, referenceAverage, bestMovement, temporaryNewMovement, preferredSlots)

      if (verbose)println(s"\t\tbestMetric = $bestMetric, peak = ${bestMovement.acc.peak}")
      if (verbose)print(s"\t\ttempMetric = $temporaryMetric, peak = ${temporaryNewMovement.acc.peak}")

      if (temporaryMetric < bestMetric) {
        if (verbose) println(" - Is best")

        bestMovement = new Movement(temporaryNewMovement.acc.copy(), temporaryNewMovement.fl, preferredSlots)
      } else {
        if (verbose) println(" - Not best")
      }
    }

    // TODO: Notice that we are returning a copy of the AccumulatedLoad, with a new mutable Set of Loads.
    bestMovement.acc

  }

  private def computeMetrics(rescheduleType: RescheduleType, referenceAverage: Double, bestMovement: Movement, temporaryNewMovement: Movement, preferedSlots: List[Int]) = {
    rescheduleType match {
      case test.RescheduleType.MinimizePeak =>
        (temporaryNewMovement.acc.peak, bestMovement.acc.peak)

      case test.RescheduleType.MinimizeMeanDistance =>
        (computeAverageDistanceMetric(referenceAverage, temporaryNewMovement, preferedSlots),
          computeAverageDistanceMetric(referenceAverage, bestMovement, preferedSlots))
        /*(computeAverageDistanceMetric(referenceAverage, computeAverageAtLoadPosition(temporaryNewMovement.acc, temporaryNewMovement.fl), tempSlotsWithPriority),
          computeAverageDistanceMetric(referenceAverage, computeAverageAtLoadPosition(bestMovement.acc, bestMovement.fl), bestSlotsWithPriority))*/

      case test.RescheduleType.BiasedPeak =>
        (temporaryNewMovement.biasedPeak, bestMovement.biasedPeak)
    }
  }



  def computeSlotsWithPriority(load: Load, preferedSlots: List[Int]): Double = {
    val howManySlots = (load.positionInT until (load.positionInT + load.span)).count(p => preferedSlots.contains(p))
    //println(s"howManySlots = $howManySlots, size = ${preferedSlots.size}")

    if (preferedSlots.isEmpty) 0.0
    else howManySlots.toDouble / preferedSlots.size.toDouble
  }

  def computeAverageDistanceMetric2(referenceAverage: Double, movement: Movement, preferedSlots: List[Int], bias: Double = 0.50): Double = {
    val actualAverage = computeBiasedAverageAtLoadPosition(movement.acc, movement.fl, preferedSlots, bias)

    val distance = Math.pow(Math.abs(referenceAverage - actualAverage), 2)

    distance
    /*var distance = 0.0
    val p = 1

    val flexLoad = movement.fl

    for (i <- flexLoad.positionInT until (flexLoad.positionInT + flexLoad.span)) {
      distance += Math.pow(Math.abs(referenceAverage - movement.acc.amplitudePerSlot(i)), p) * (1 - bias)
    }

    Math.pow(distance, 1 / p) * (1 - bias)*/
  }

  def computeAverageDistanceMetric(referenceAverage: Double, movement: Movement, preferedSlots: List[Int], bias: Double = 0.50): Double = {
    val acc = movement.acc
    val flexibleLoad = movement.fl

    val actualAverage = computeAverageAtLoadPosition(acc, flexibleLoad)
    val slotsWithPriority = computeSlotsWithPriority(flexibleLoad, preferedSlots)

    val distance = Math.pow(Math.abs(referenceAverage - actualAverage), 1)
    println(s"average_ref = $referenceAverage, bias = $bias, slotsWithPriority = $slotsWithPriority")

    distance * (1 - bias * slotsWithPriority)
  }

  def computeBiasedAverageAtLoadPosition(accumulatedLoad: SpanSlotAccumulatedLoad, load: Load, preferedSlots: List[Int], bias: Double): Double = {
    val fromSlot = load.positionInT
    val untilSlot = load.positionInT + load.span

    var sum = 0.0
    for (i <- fromSlot until untilSlot) {
      val amplitude = accumulatedLoad.amplitudePerSlot(i)
      val biasedAmplitude = {
        if (preferedSlots.contains(i)) amplitude * bias
        else amplitude
      }
      sum += biasedAmplitude
    }

    val average = sum / load.span

    average
  }

  def computeAverageAtLoadPosition(accumulatedLoad: SpanSlotAccumulatedLoad, load: Load): Double = {
    val fromSlot = load.positionInT
    val untilSlot = load.positionInT + load.span

    val average = accumulatedLoad.amplitudePerSlot.slice(fromSlot, untilSlot).sum / load.span

    average
  }

  def isLoadOnPreferedSlots(load: Load, preferedSlots: List[Int]): Boolean = {
    val flRange = for (i <- load.positionInT until (load.positionInT + load.span)) yield i

    flRange.forall(preferedSlots.contains(_))
  }

}

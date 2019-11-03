package scheduler_model.user_allocator.user_representation

import scheduler_model.load.{AccumulatedLoad, FlexibleLoadRepresentation}

object RepresentUserAsAmplitude extends ((FlexibleLoadRepresentation, Int) => Vector[Double]) {

  def apply(user: FlexibleLoadRepresentation, windowSize: Int): Vector[Double] = {

    if (windowSize == 0) return Vector.empty[Double]

    val totalEnergyFromFlexibleLoads: Double = user.amplitude
    val windowSlotAmplitude = totalEnergyFromFlexibleLoads / windowSize
    val flexibleLoadVector: Vector[Double] = (for (_ <- 0 until windowSize) yield {
      windowSlotAmplitude
    }).toVector

    flexibleLoadVector

  }

}

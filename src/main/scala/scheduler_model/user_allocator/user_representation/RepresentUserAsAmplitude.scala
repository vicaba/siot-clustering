package scheduler_model.user_allocator.user_representation

import scheduler_model.load.AccumulatedLoad

object RepresentUserAsAmplitude extends ((AccumulatedLoad, Int) => Vector[Double]) {

  def apply(user: AccumulatedLoad, windowSize: Int): Vector[Double] = {

    if (windowSize == 0) return Vector.empty[Double]

    val totalEnergyFromFlexibleLoads: Double = user.flexibleLoads.foldLeft(0.0)(_ + _.totalEnergy)
    val windowSlotAmplitude = totalEnergyFromFlexibleLoads / windowSize
    val flexibleLoadVector: Vector[Double] = (for (_ <- 0 until windowSize) yield {
      windowSlotAmplitude
    }).toVector

    flexibleLoadVector

  }

}

package test

import test.load.{AccumulatedLoad, FlexibleLoad, Load}
import test.reschedulermetrics.NoTransformation

object UserAllocator {


  /**
   * Allocates users along the complete timespan. The algorithm "transforms" each user as a flexible load of windowSize and
   * consumption per slot flexibleLoad.totalEnergy / windowSize. Then it uses the rescheduler to assign each user to the best timeslots.+
   *
   * @param users
   * @param numberOfSlots
   * @param windowSize
   * @return A list in the same order as users with schedulerPreferredTimeSlots per each user as an inner List[Int]
   */
  def allocate(users: List[AccumulatedLoad], numberOfSlots: Int, windowSize: Int): List[List[Int]] = {

    val loadListOrderingByAmplitude: Ordering[List[Load]] =
      (x: List[Load], y: List[Load]) => implicitly[Ordering[Double]].compare(x.map(_.totalEnergy).sum, y.map(_.totalEnergy).sum)

    loadListOrderingByAmplitude.on[AccumulatedLoad](_.flexibleLoads.toList)

    //users.sorted


    // TODO: Test this by comparing results of BenchmarkSpec and SchedulerSpec
    val sortedUsers = users.sorted(loadListOrderingByAmplitude.on[AccumulatedLoad](_.flexibleLoads.toList)).reverse

    //val sortedUsers = users.sortBy(_.flexibleLoads.toList.map(_.totalEnergy).sum).reverse

    val usersAsFlexibleLoads = for (user <- sortedUsers) yield {

      val totalEnergyFromFlexibleLoads: Double = user.flexibleLoads.foldLeft(0.0)(_ + _.amplitudePerSlot.sum)

      val flexibleLoadVector: Vector[Double] = (for (_ <- 0 until windowSize) yield {
        totalEnergyFromFlexibleLoads / windowSize
      }).toVector

      FlexibleLoad(user.id, 0, flexibleLoadVector)

    }

    val fixedLoads = sortedUsers.flatMap(_.fixedLoads)

    val lowestPositionInT = sortedUsers.flatMap(_.loads).map(_.positionInT).min
    val accumulatedLoads =
      AccumulatedLoad.keepLoadOrder(0, lowestPositionInT, fixedLoads ::: usersAsFlexibleLoads)

    val allocationResult = SchedulerAlgorithm.reschedule(accumulatedLoads, metricTransformation = NoTransformation)

    // Reorder per "users" input
    // TODO: This can be optimized
    val orderedAllocationResult = for {
      userId             <- users.map(_.id)
      userAsFlexibleLoad <- allocationResult.flexibleLoads
      if userId == userAsFlexibleLoad.id
    } yield userAsFlexibleLoad

    orderedAllocationResult.map { userAsFlexibleLoad =>
      (for (i <- userAsFlexibleLoad.positionInT until (userAsFlexibleLoad.positionInT + windowSize)) yield i).toList
    }

  }
}

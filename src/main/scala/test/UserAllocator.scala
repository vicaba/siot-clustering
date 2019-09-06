package test

import test.load.{AccumulatedLoad, FlexibleLoad, Load}
import test.reschedulermetrics.NoTransformation

object UserAllocator {

  val DefaultOrderings: List[Ordering[AccumulatedLoad]] = List(
    Load.loadListOrderingByAmplitude.reverse,
    Load.loadListOrderingByAmplitude,
    Load.loadListOrderingByMaxPositionInT.reverse,
    Load.loadListOrderingByMaxPositionInT,
  ).map(_.on[AccumulatedLoad](_.flexibleLoads.toList))

  /**
    * Allocates users along the complete timespan. The algorithm "transforms" each user as a flexible load of windowSize and
    * consumption per slot flexibleLoad.totalEnergy / windowSize. Then it uses the rescheduler to assign each user to the best timeslots.+
    *
    * @param users
    * @param numberOfSlots
    * @param windowSize
    * @return A list in the same order as users with schedulerPreferredTimeSlots per each user as an inner List[Int]
    */
  def allocate(users: List[AccumulatedLoad],
               numberOfSlots: Int,
               windowSize: Int,
               userOrdering: Ordering[AccumulatedLoad] =
                 Load.loadListOrderingByAmplitude.on[AccumulatedLoad](_.flexibleLoads.toList).reverse): List[List[Int]] = {

    // TODO: Test this by comparing results of BenchmarkSpec and SchedulerSpec
    val sortedUsers = users.sorted(userOrdering)

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
      AccumulatedLoad.keepLoadOrder(0, lowestPositionInT, fixedLoads ::: usersAsFlexibleLoads, "")

    val allocationResult = SchedulerAlgorithm.reschedule(accumulatedLoads, metricTransformation = NoTransformation)

    // Reorder per "users" input
    val order = users.map(_.id)
    val allocationResultFlexibleLoads = allocationResult.flexibleLoads.toList
    val allocationResultAsMap = allocationResultFlexibleLoads.map(_.id).zip(allocationResultFlexibleLoads).toMap
    val orderedAllocationResult = order.map(allocationResultAsMap)


    orderedAllocationResult.map { userAsFlexibleLoad =>
      (for (i <- userAsFlexibleLoad.positionInT until (userAsFlexibleLoad.positionInT + windowSize)) yield i).toList
    }

  }
}

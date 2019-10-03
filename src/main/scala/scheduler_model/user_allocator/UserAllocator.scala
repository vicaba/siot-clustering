package scheduler_model.user_allocator

import breeze.linalg.DenseVector
import scheduler_model.load._
import scheduler_model.scheduler.SchedulerAlgorithm
import scheduler_model.scheduler.metric_transformer.NoTransformation
import types.clusterer.DataTypeMetadata

object UserAllocator {

  val DefaultOrderings: List[Ordering[AccumulatedLoad]] = List(
    Load.loadListOrderingByAmplitude.reverse,
    Load.loadListOrderingByAmplitude,
    Load.loadListOrderingByMaxPositionInT.reverse,
    Load.loadListOrderingByMaxPositionInT,
  ).map(_.on[AccumulatedLoad](_.flexibleLoads.toList))

  val DefaultOrdering = Load.loadListOrderingByAmplitude.reverse.on[AccumulatedLoad](_.flexibleLoads.toList)

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
    userOrdering: Ordering[AccumulatedLoad] = DefaultOrdering) = {

    val amplitudePerSlotMetadata = users.head.amplitudePerSlotMetadata


    // TODO: Test this by comparing results of BenchmarkSpec and SchedulerSpec
    val sortedUsers = users.sorted(userOrdering)

    val usersAsFlexibleLoads = for (user <- sortedUsers) yield {

      val totalEnergyFromFlexibleLoads: Double = user.flexibleLoads.foldLeft(0.0)(_ + _.totalEnergy)
      val windowSlotAmplitude = totalEnergyFromFlexibleLoads / windowSize
      val flexibleLoadVector: Vector[Double] = (for (_ <- 0 until windowSize) yield {
        windowSlotAmplitude
      }).toVector

      FlexibleLoad(user.id,
        user.id,
        "User as FlexibleLoad",
        0,
        DenseVector(flexibleLoadVector.toArray))(DataTypeMetadata.generateDataTypeMetadata(forColumns = flexibleLoadVector.size))
    }

    val fixedLoads = sortedUsers.flatMap(_.fixedLoads)

    val accumulatedLoads =
      AccumulatedLoad.keepLoadOrder(0, 0, "AccumulatedLoad with users", fixedLoads ::: usersAsFlexibleLoads)(amplitudePerSlotMetadata)

    val allocationResult = SchedulerAlgorithm.reschedule(accumulatedLoads, metricTransformation = NoTransformation, verbose = true)

    // Reorder per "users" input
    val order = users.map(_.id)
    val allocationResultFlexibleLoads = allocationResult.flexibleLoads.toList
    val allocationResultAsMap = allocationResultFlexibleLoads.map(_.id).zip(allocationResultFlexibleLoads).toMap
    val orderedAllocationResult = order.map(allocationResultAsMap)


    orderedAllocationResult.map { userAsFlexibleLoad =>
      (for (i <- userAsFlexibleLoad.startPositionInTime until (userAsFlexibleLoad.startPositionInTime + windowSize)) yield i).toList
    }
  }
}


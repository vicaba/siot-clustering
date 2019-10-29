package scheduler_model.user_allocator

import breeze.linalg.{DenseVector, max}
import scheduler_model.load._
import scheduler_model.scheduler.SchedulerAlgorithm
import scheduler_model.scheduler.metric_transformer.NoTransformation
import types.clusterer.DataTypeMetadata

import scala.util.Try

object UserAllocator {

  val DefaultOrderings: List[Ordering[AccumulatedLoad]] = List(
    Load.loadListOrderingByAmplitude.reverse,
    Load.loadListOrderingByAmplitude,
    Load.loadListOrderingByMaxPositionInT.reverse,
    Load.loadListOrderingByMaxPositionInT,
  ).map(_.on[AccumulatedLoad](_.flexibleLoads.toList))

  val DefaultOrdering = Load.loadListOrderingByAmplitude.reverse.on[AccumulatedLoad](_.flexibleLoads.toList)

  def representUsersAsFlexibleLoadsInAccumulatedLoad(
      users: List[AccumulatedLoad]): AccumulatedLoad = {
    val amplitudePerSlotMetadata = users.head.amplitudePerSlotMetadata
    val fixedLoads               = users.flatMap(_.fixedLoads)
    val accumulatedLoadWithFixedLoadsOnly =
      AccumulatedLoad(0, 0, "AccumulatedLoad with fixed loads of users", fixedLoads)(amplitudePerSlotMetadata)

    def representUserAsAmplitude(user: AccumulatedLoad, windowSize: Int): Vector[Double] = {

      if (windowSize == 0) return Vector.empty[Double]

      val totalEnergyFromFlexibleLoads: Double = user.flexibleLoads.foldLeft(0.0)(_ + _.totalEnergy)
      val windowSlotAmplitude                  = totalEnergyFromFlexibleLoads / windowSize
      val flexibleLoadVector: Vector[Double] = (for (_ <- 0 until windowSize) yield {
        windowSlotAmplitude
      }).toVector

      flexibleLoadVector

    }

    val usersAsFlexibleLoads = for (user <- users) yield {

      val amplitudeInMinimumTimeSpan = representUserAsAmplitude(user, windowSize = Try(user.flexibleLoads.toList.map(_.span).max).getOrElse(0))

      val userAsAmplitude = if (Try(amplitudeInMinimumTimeSpan.max).getOrElse(0.0) >= max(accumulatedLoadWithFixedLoadsOnly.amplitudePerSlot)) {
        val amplitudeInMaximumTimeSpan = representUserAsAmplitude(user, windowSize = Try(user.flexibleLoads.toList.map(_.span).sum).getOrElse(0))
        amplitudeInMaximumTimeSpan
      } else amplitudeInMinimumTimeSpan

      FlexibleLoad(user.id, user.id, "User as FlexibleLoad", 0, DenseVector(userAsAmplitude.toArray))

    }

    AccumulatedLoad.keepLoadOrder(0, 0, "AccumulatedLoad with users", fixedLoads ::: usersAsFlexibleLoads)(
      amplitudePerSlotMetadata)
  }

  /**
    * Allocates users along the complete timespan. The algorithm "transforms" each user as a flexible load of windowSize and
    * consumption per slot flexibleLoad.totalEnergy / windowSize. Then it uses the rescheduler to assign each user to the best timeslots.+
    *
    * @param users
    * @return A list in the same order as users with schedulerPreferredTimeSlots per each user as an inner List[Int]
    */
  def allocate(users: List[AccumulatedLoad],
               userOrdering: Ordering[AccumulatedLoad] = DefaultOrdering): List[List[Int]] = {

    // TODO: Test this by comparing results of BenchmarkSpec and SchedulerSpec
    val sortedUsers = users.sorted(userOrdering)


    val accumulatedLoads = representUsersAsFlexibleLoadsInAccumulatedLoad(sortedUsers)

    val allocationResult = SchedulerAlgorithm.reschedule(accumulatedLoads, metricTransformation = NoTransformation)

    // Reorder per "users" input
    val order                         = users.map(_.id)
    val allocationResultFlexibleLoads = allocationResult.flexibleLoads.toList
    val allocationResultAsMap         = allocationResultFlexibleLoads.map(_.id).zip(allocationResultFlexibleLoads).toMap
    val orderedAllocationResult       = order.map(allocationResultAsMap)

    orderedAllocationResult.map { userAsFlexibleLoad =>
      (for (i <- userAsFlexibleLoad.startPositionInTime until (userAsFlexibleLoad.startPositionInTime + userAsFlexibleLoad.span))
        yield i).toList
    }
  }
}

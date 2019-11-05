package scheduler_model.user_allocator

import breeze.linalg.{DenseVector, max, sum}
import scheduler_model.load._
import scheduler_model.scheduler.SchedulerAlgorithm
import scheduler_model.scheduler.metric_transformer.NoTransformation
import scheduler_model.user_allocator.user_representation.conditions.MaxPeakGraterThanMaxFixedLoadsPeakCondition
import scheduler_model.user_allocator.user_representation.{
  UserRepresentationAsAmplitude,
  UserRepresentationAsAmplitudeInMaxTimeSpan,
  UserRepresentationAsAmplitudeInMinTimeSpan
}
import types.clusterer.DataTypeMetadata

import scala.util.Try

object UserAllocator {

  val DefaultOrderings: List[Ordering[AccumulatedLoad]] = List(
    Load.loadListOrderingByAmplitude.reverse,
    Load.loadListOrderingByAmplitude,
    Load.loadListOrderingByMaxPositionInT.reverse,
    Load.loadListOrderingByMaxPositionInT,
  ).map(_.on[AccumulatedLoad](_.flexibleLoads.toList))

  val DefaultOrdering: Ordering[AccumulatedLoad] =
    Load.loadListOrderingByAmplitude.reverse.on[AccumulatedLoad](_.flexibleLoads.toList)

  val DefaultUserRepresentationAsAmplitude: UserRepresentationAsAmplitudeInMinTimeSpan =
    new UserRepresentationAsAmplitudeInMinTimeSpan(
      Some(MaxPeakGraterThanMaxFixedLoadsPeakCondition, new UserRepresentationAsAmplitudeInMaxTimeSpan()))

  def representUserAsFlexibleLoadRepresentationsInAccumulatedLoad(
      users: List[AccumulatedLoad]
  ): AccumulatedLoad = {

    val amplitudePerSlotMetadata = users.head.amplitudePerSlotMetadata
    val fixedLoads               = users.flatMap(_.fixedLoads)

    val usersAsFlexibleLoads = for (user <- users) yield {

      val userAmplitude       = Try(sum(sum(user.flexibleLoads.toList.map(_.amplitudePerSlot)))).getOrElse(0.0)
      val userMinLoadTimeSpan = Try(user.flexibleLoads.toList.map(_.span).max).getOrElse(0)
      val userMaxTimeSpan     = Try(user.flexibleLoads.toList.map(_.span).sum).getOrElse(0)

      FlexibleLoadRepresentation(user.id,
                                 user.id,
                                 "User as FlexibleLoad",
                                 userAmplitude,
                                 userMaxTimeSpan,
                                 userMinLoadTimeSpan)

    }

    AccumulatedLoad.keepLoadOrder(0, 0, "AccumulatedLoad with users", fixedLoads ::: usersAsFlexibleLoads)(
      amplitudePerSlotMetadata)

  }

  def representUserAsBestFlexibleLoad(userRepresentationAsAmplitude: UserRepresentationAsAmplitude)(
      acc: => AccumulatedLoad,
      xy: (FlexibleLoad, FlexibleLoad)): (FlexibleLoad, FlexibleLoad) = xy._1 match {
    case x: FlexibleLoadRepresentation =>
      val newX = userRepresentationAsAmplitude(x, acc)
      val fl   = FlexibleLoad(x.id, x.id, "User as FlexibleLoad", 0, DenseVector(newX.toArray))
      (fl, fl.copy())
    case _ => xy
  }

  /*  def representUsersAsFlexibleLoadsInAccumulatedLoad(
      users: List[AccumulatedLoad],
      userRepresentationAsAmplitude: UserRepresentationAsAmplitude): AccumulatedLoad = {

    val amplitudePerSlotMetadata = users.head.amplitudePerSlotMetadata
    val fixedLoads               = users.flatMap(_.fixedLoads)
    val accumulatedLoadWithFixedLoadsOnly =
      AccumulatedLoad(0, 0, "AccumulatedLoad with fixed loads of users", fixedLoads)(amplitudePerSlotMetadata)

    val usersAsFlexibleLoads = for (user <- users) yield {

      val userAsAmplitude = userRepresentationAsAmplitude(user, accumulatedLoadWithFixedLoadsOnly)

      FlexibleLoad(user.id, user.id, "User as FlexibleLoad", 0, DenseVector(userAsAmplitude.toArray))

    }

    AccumulatedLoad.keepLoadOrder(0, 0, "AccumulatedLoad with users", fixedLoads ::: usersAsFlexibleLoads)(
      amplitudePerSlotMetadata)
  }*/

  /**
    * Allocates users along the complete timespan. The algorithm "transforms" each user as a flexible load of windowSize and
    * consumption per slot flexibleLoad.totalEnergy / windowSize. Then it uses the rescheduler to assign each user to the best timeslots.+
    *
    * @param users
    * @return A list in the same order as users with schedulerPreferredTimeSlots per each user as an inner List[Int]
    */
  def allocate(users: List[AccumulatedLoad],
               userOrdering: Ordering[AccumulatedLoad] = DefaultOrdering,
               userRepresentationAsAmplitude: UserRepresentationAsAmplitude = DefaultUserRepresentationAsAmplitude)
    : List[List[Int]] = {

    // TODO: Test this by comparing results of BenchmarkSpec and SchedulerSpec
    val sortedUsers = users.sorted(userOrdering)

    val accumulatedLoads = representUserAsFlexibleLoadRepresentationsInAccumulatedLoad(sortedUsers)

    val allocationResult = SchedulerAlgorithm.reschedule(
      accumulatedLoads,
      metricTransformation = NoTransformation,
      flexibleLoadTransformer = representUserAsBestFlexibleLoad(userRepresentationAsAmplitude))

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

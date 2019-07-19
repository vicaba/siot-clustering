package test

import test.reschedulermetrics.NoTransformation

class UserAllocator {}

object UserAllocator {

  def allocate(users: List[SpanSlotAccumulatedLoad], numOfSlots: Int, windowSize: Int): List[List[Int]] = {

    val sortedUsers = users.sortBy(_.flexibleLoads.toList.map(_.totalEnergy).sum).reverse

    val usersAsFlexibleLoads = for (user <- sortedUsers) yield {

      val totalEnergyFromFlexibleLoads: Double = user.flexibleLoads.foldLeft(0.0)(_ + _.amplitudePerSlot.sum)

      val flexibleLoadVector: Vector[Double] = (for (_ <- 0 until windowSize) yield {
        totalEnergyFromFlexibleLoads / windowSize
      }).toVector

      SpanSlotFlexibleLoad(user.id, 0, flexibleLoadVector)

    }

    val fixedLoads = sortedUsers.flatMap(_.fixedLoads)

    val lowestPositionInT = sortedUsers.flatMap(_.loads).map(_.positionInT).min
    val accumulatedLoads =
      SpanSlotAccumulatedLoad.keepLoadOrder(0, lowestPositionInT, fixedLoads ::: usersAsFlexibleLoads)

    val result = Rescheduler.reschedule(accumulatedLoads, metricTransformation = NoTransformation)

    val schedulerPreferredSlotsForEachUser: List[List[Int]] = for {
      userId <- users.map(_.id)
      userAsFlexibleLoad <- result.flexibleLoads
      if userId == userAsFlexibleLoad.id
    } yield {
      val userPreferredSlots = (for (i <- userAsFlexibleLoad.positionInT until (userAsFlexibleLoad.positionInT + windowSize)) yield i).toList
      userPreferredSlots
    }

    schedulerPreferredSlotsForEachUser

  }
}

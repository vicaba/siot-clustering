package test

import test.reschedulermetrics.NoTransformation

class UserAllocator2 {}

object UserAllocator2 {

  def allocate(users: List[SpanSlotAccumulatedLoad], numOfSlots: Int, windowSize: Int): List[List[Int]] = {

    val sortedUsers = users.sortBy(_.flexibleLoads.toList.map(_.totalEnergy).sum).reverse

    val usersAsFlexibleLoads = (for (user <- sortedUsers) yield {

      val totalEnergyFromFlexibleLoads: Double = user.flexibleLoads.foldLeft(0.0)(_ + _.amplitudePerSlot.sum)

      val flexibleLoadVector: Vector[Double] = (for (_ <- 0 until windowSize) yield {
        totalEnergyFromFlexibleLoads / windowSize
      }).toVector

      SpanSlotFlexibleLoad(user.id, 0, flexibleLoadVector)

    }).reverse

    val fixedLoads = sortedUsers.flatMap(_.fixedLoads)

    val accumulatedLoads =
      SpanSlotAccumulatedLoad.keepLoadOrder(0,
        sortedUsers.flatMap(_.loads).map(_.positionInT).min,
        fixedLoads ::: usersAsFlexibleLoads)

    val result = Rescheduler.reschedule(accumulatedLoads, metricTransformation = NoTransformation)

    var usersPreferedSlots: List[List[Int]] = List()
    for (userId <- users.map(_.id)) {
      for (flexUser <- result.flexibleLoads) {
        if (userId == flexUser.id) {
          var userPreferedSlots: List[Int] = List()
          for (i <- flexUser.positionInT until (flexUser.positionInT + windowSize)) {
            userPreferedSlots = i :: userPreferedSlots
          }
          usersPreferedSlots = userPreferedSlots.reverse :: usersPreferedSlots
        }
      }
    }

    usersPreferedSlots.reverse
  }
}

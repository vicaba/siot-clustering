package test

class UserAllocator {

}

object UserAllocator {
  def allocate(users: List[SpanSlotAccumulatedLoad], numOfSlots: Int, slotsWindowSize: Int): List[List[Int]] = {
    var usersAsFlexibleLoads: List[SpanSlotFlexibleLoad] = List()

    for (user <- users) {
      //TODO: arreglar esto, ya funciona pero es un poco feo
      val rawUserFlexibleLoad = {
        var sum = 0.0
        for (flexibleLoad <- user.flexibleLoads) {
          sum += flexibleLoad.amplitudePerSlot.sum
        }
        sum
      }

      val flexibleLoadVector: Vector[Double] = (for (_ <- 0 until slotsWindowSize) yield {
        rawUserFlexibleLoad / slotsWindowSize
      }).toVector

      usersAsFlexibleLoads = SpanSlotFlexibleLoad(user.id, 0, flexibleLoadVector) :: usersAsFlexibleLoads
    }

    val fixedLoads = users.flatMap(_.fixedLoads)
    val accumulatedLoads = SpanSlotAccumulatedLoad(0, users.flatMap(_.loads).map(_.positionInT).min, fixedLoads ::: usersAsFlexibleLoads)

    val result = Rescheduler.reschedule(accumulatedLoads, rescheduleType = RescheduleType.MinimizePeak)

    var usersPreferedSlots: List[List[Int]] = List()
    for (userId <- users.map(_.id)) {
      for (flexUser <- result.flexibleLoads) {
        if (userId == flexUser.id) {
          var userPreferedSlots: List[Int] = List()
          for (i <- flexUser.positionInT until (flexUser.positionInT + slotsWindowSize)) {
            userPreferedSlots = i :: userPreferedSlots
          }
          usersPreferedSlots = userPreferedSlots.reverse :: usersPreferedSlots
        }
      }
    }

    usersPreferedSlots
  }
}

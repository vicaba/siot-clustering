package test

class UserAllocator {

}

object UserAllocator {
  def allocate(users: List[SpanSlotAccumulatedLoad], numOfSlots: Int, slotsWindowSize: Int): SpanSlotAccumulatedLoad = {
    var usersAsFlexibleLoads: List[SpanSlotFlexibleLoad] = List()

    for (user <- users) {
      //TODO: arreglar esto porque hay un lio entre sets y vectores
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
    val accumulatedLoads = SpanSlotAccumulatedLoad(users.flatMap(_.loads).map(_.positionInT).min, fixedLoads ::: usersAsFlexibleLoads)

    Rescheduler.reschedule(accumulatedLoads)
  }
}

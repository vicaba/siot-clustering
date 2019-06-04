package test

class UserAllocator {

}

object UserAllocator {
  def allocate(users: List[SpanSlotAccumulatedLoad], numOfSlots: Int, slotsWindowSize: Int): SpanSlotAccumulatedLoad = {
    var usersAsFlexibleLoads: List[SpanSlotFlexibleLoad] = List()

    for (user <- users) {
      val rawUserLoad = user.accumulatedLoads.flatMap(_.flexibleLoads.flatMap(_.amplitudePerSlot)).sum

      val loadVector: Vector[Double] = (for (_ <- 0 until slotsWindowSize) yield {
        rawUserLoad / numOfSlots
      }).toVector

      usersAsFlexibleLoads = SpanSlotFlexibleLoad(user.id, 0, loadVector) :: usersAsFlexibleLoads
    }

    val fixedLoads = users.flatMap(_.fixedLoads)
    val accumulatedLoads = SpanSlotAccumulatedLoad(-1, fixedLoads ::: usersAsFlexibleLoads)

    Rescheduler.reschedule(accumulatedLoads)
  }
}

package test

import types.immutable.Point

class UserAllocator {

}

object UserAllocator {
  def allocate(users: List[SpanSlotAccumulatedLoad], numOfSlots: Int, slotsWindowSize: Int): List[Int] = {
    var positions: List[Int] = List()

    val fixedLoadPerSlots: Array[Double] = Array.fill(numOfSlots){0.0}
    val averageLoadPerSlot = users.map(_.amplitudePerSlot.sum).sum / numOfSlots

    users.foreach(_.fixedLoads.foreach(fixedLoad => {
      val positionInT = fixedLoad.positionInT
      val amplitudePerSlot = fixedLoad.amplitudePerSlot
      for (i <- amplitudePerSlot.indices) {
        fixedLoadPerSlots(positionInT + i ) += amplitudePerSlot(i)
      }
    }))

    //Rescheduler.rescheduleFlexibleLoad()





    positions
  }
}

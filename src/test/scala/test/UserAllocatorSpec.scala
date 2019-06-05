package test

import org.scalatest.{FeatureSpec, GivenWhenThen}

class UserAllocatorSpec extends FeatureSpec with GivenWhenThen {

  feature("Allocating users") {

    scenario("Test scenario") {
      val usersSimulation: List[SpanSlotAccumulatedLoad] = List(
        SpanSlotAccumulatedLoad(100, 0, List(
          SpanSlotFixedLoad(101, 0, Vector(1, 5, 1)),
          SpanSlotFlexibleLoad(151, 0, Vector(1, 1, 1))
        )),

        SpanSlotAccumulatedLoad(200, 0, List(
          SpanSlotFixedLoad(201, 0, Vector(0, 0, 0)),
          SpanSlotFlexibleLoad(251, 0, Vector(3, 1, 1))
        )),

        SpanSlotAccumulatedLoad(300, 0, List(
          SpanSlotFixedLoad(301, 0, Vector(0, 0, 0)),
          SpanSlotFlexibleLoad(351, 0, Vector(1, 1, 1))
        ))
      )

      val usersPreferedSlots = UserAllocator.allocate(usersSimulation, 3, 1)

      for (i <- usersSimulation.indices) {
        println(s"User ${usersSimulation(i).id} at position ${usersPreferedSlots(i).head}")
      }
    }
  }
}

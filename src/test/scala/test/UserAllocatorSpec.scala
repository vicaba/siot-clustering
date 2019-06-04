package test

import org.scalatest.{FeatureSpec, GivenWhenThen}

class UserAllocatorSpec extends FeatureSpec with GivenWhenThen {

  feature("Allocating users") {

    scenario("Test scenario") {
      val usersSimulation: List[SpanSlotAccumulatedLoad] = List(
        SpanSlotAccumulatedLoad(100, List(
          SpanSlotFixedLoad(101, 0, Vector(5, 5, 5, 5, 2, 2, 2)),
          SpanSlotFlexibleLoad(151, 0, Vector(1, 2, 2))
        )),

        SpanSlotAccumulatedLoad(200, List(
          SpanSlotFixedLoad(201, 0, Vector(5, 5, 5, 5, 2, 2, 2)),
          SpanSlotFlexibleLoad(251, 0, Vector(1, 2, 2))
        ))
      )

      val result = UserAllocator.allocate(usersSimulation, 7, 1)
      result.flexibleLoads.foreach(flex => {
        println(s"User ${flex.id} at position ${flex.positionInT}")
      })
    }
  }
}

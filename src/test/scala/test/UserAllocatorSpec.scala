package test

import org.scalatest.{FeatureSpec, GivenWhenThen}

class UserAllocatorSpec extends FeatureSpec with GivenWhenThen {

  feature("Allocating users") {

    scenario("Test scenario") {
      val usersSimulation: List[SpanSlotAccumulatedLoad] = List(
        SpanSlotAccumulatedLoad(100, List(
          SpanSlotFixedLoad(101, 0, Vector(1, 2, 1)),
          SpanSlotFlexibleLoad(151, 0, Vector(1, 1, 1))
        )),

        SpanSlotAccumulatedLoad(200, List(
          SpanSlotFixedLoad(201, 0, Vector(0, 0, 0)),
          SpanSlotFlexibleLoad(251, 0, Vector(1, 1, 1))
        ))
      )

      val result = UserAllocator.allocate(usersSimulation, 3, 1)
      result.flexibleLoads.foreach(flex => {
        println(s"User ${flex.id} at position ${flex.positionInT}")
      })
    }
  }
}

package test

import org.scalatest.{FeatureSpec, GivenWhenThen}
import test.load.{AccumulatedLoad, FixedLoad, FlexibleLoad}

class UserAllocatorSpec extends FeatureSpec with GivenWhenThen {

  feature("Allocating users") {

    scenario("Test scenario") {
      val usersSimulation: List[AccumulatedLoad] = List(
        AccumulatedLoad(100, 0, List(
          FixedLoad(101, 0, Vector(1, 5, 1)),
          FlexibleLoad(151, 0, Vector(1, 1, 1))
        )),

        AccumulatedLoad(200, 0, List(
          FixedLoad(201, 0, Vector(0, 0, 0)),
          FlexibleLoad(251, 0, Vector(3, 1, 1))
        )),

        AccumulatedLoad(300, 0, List(
          FixedLoad(301, 0, Vector(0, 0, 0)),
          FlexibleLoad(351, 0, Vector(1, 1, 1))
        ))
      )

      val usersPreferedSlots = UserAllocator.allocate(usersSimulation, 3, 1)

      for (i <- usersSimulation.indices) {
        println(s"User ${usersSimulation(i).id} at position ${usersPreferedSlots(i).head}")
      }
    }
  }
}

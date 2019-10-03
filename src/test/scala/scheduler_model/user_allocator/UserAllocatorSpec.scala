package scheduler_model.user_allocator

import breeze.linalg.DenseVector
import org.scalatest.{FeatureSpec, GivenWhenThen}
import scheduler_model.load._
import types.clusterer.DataTypeMetadata

class UserAllocatorSpec extends FeatureSpec with GivenWhenThen {

  feature("Allocating users") {

    scenario("Test scenario") {

      implicit val amplitudePerSlotMetadata = DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)

      val usersSimulation: List[AccumulatedLoad] = List(
        AccumulatedLoad(100, 100, "100", List(
          FixedLoad(101, 101, "101", DenseVector(1, 5, 1)),
          FlexibleLoad(151, 151, "151", 0, DenseVector(1, 1, 1))
        )),

        AccumulatedLoad(200, 200, "200", List(
          FixedLoad(201, 201, "201", DenseVector(0, 0, 0)),
          FlexibleLoad(251, 251, "251", 0, DenseVector(3, 1, 1))
        )),

        AccumulatedLoad(300, 300, "300", List(
          FixedLoad(301, 301, "301", DenseVector(0, 0, 0)),
          FlexibleLoad(351, 351, "351", 0, DenseVector(1, 1, 1))
        ))
      )

      val usersPreferredSlots = UserAllocator.allocate(usersSimulation, 3, 1)

      for (i <- usersSimulation.indices) {
        println(s"User ${usersSimulation(i).id} at position ${usersPreferredSlots(i).head}")
      }
    }
  }
}

package scheduler_model.user_allocator

import breeze.linalg.DenseVector
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import org.scalatest.Inspectors._
import scheduler_model.load.{FlexibleLoad, _}
import scheduler_model.user_allocator.user_representation.{
  UserRepresentationAsAmplitudeInMaxTimeSpan,
  UserRepresentationAsAmplitudeInMinTimeSpan
}
import scheduler_model.user_allocator.user_representation.conditions.MaxPeakGraterThanMaxFixedLoadsPeakCondition
import types.clusterer.DataTypeMetadata

class UserAllocatorSpec extends FeatureSpec with GivenWhenThen {

  feature("UserAllocator allocates users with best effort") {

    scenario(
      "User with two flexible loads spanning 1 time-slot, should not be spread in time if min user representation is provided only") {

      implicit val amplitudePerSlotMetadata: DataTypeMetadata =
        DataTypeMetadata.generateDataTypeMetadata(forColumns = 5)

      Given("A user with one fixed load and two flexible loads")

      val usersSimulation: List[AccumulatedLoad] = List(
        AccumulatedLoad(
          100,
          100,
          "100",
          List(FixedLoad(101, 101, "101", DenseVector(1, 2, 2, 1, 1)),
               FlexibleLoad(151, 151, "151", 0, DenseVector(1)),
               FlexibleLoad(151, 152, "152", 0, DenseVector(1)))
        ))

      When("Allocating user with only UserRepresentationAsAmplitudeInMinTimeSpan as heuristic")

      val usersPreferredSlots = UserAllocator.allocate(usersSimulation,
                                                       userRepresentationAsAmplitude =
                                                         new UserRepresentationAsAmplitudeInMinTimeSpan())

      Then("the flexible loads should not be spread in time")

      usersPreferredSlots shouldNot contain(List(3, 4))

    }

    scenario(
      "User with two flexible loads spanning 1 time-slot, should be spread in time when min and max user representations are provided") {

      implicit val amplitudePerSlotMetadata: DataTypeMetadata =
        DataTypeMetadata.generateDataTypeMetadata(forColumns = 5)

      Given("A user with one fixed load and two flexible loads")

      val usersSimulation: List[AccumulatedLoad] = List(
        AccumulatedLoad(
          100,
          100,
          "100",
          List(FixedLoad(101, 101, "101", DenseVector(1, 2, 2, 1, 1)),
               FlexibleLoad(151, 151, "151", 0, DenseVector(1)),
               FlexibleLoad(151, 152, "152", 0, DenseVector(1)))
        ))

      When(
        "Allocating user with UserRepresentationAsAmplitudeInMinTimeSpan and UserRepresentationAsAmplitudeInMaxTimeSpan as heuristic")

      val userRepresentationAsAmplitude = new UserRepresentationAsAmplitudeInMinTimeSpan(
        Some(MaxPeakGraterThanMaxFixedLoadsPeakCondition, new UserRepresentationAsAmplitudeInMaxTimeSpan()))

      val usersPreferredSlots =
        UserAllocator.allocate(usersSimulation, userRepresentationAsAmplitude = userRepresentationAsAmplitude)

      Then("the flexible loads should be spread in time")

      usersPreferredSlots should contain(List(3, 4))

    }

    scenario("User with two flexible loads spanning 1 time-slot, should be spread in time when min and max user representations are provided (test2)") {

      implicit val amplitudePerSlotMetadata: DataTypeMetadata = DataTypeMetadata.generateDataTypeMetadata(forColumns = 5)

      Given("A user with one fixed load and two flexible loads")

      val usersSimulation: List[AccumulatedLoad] = List(
        AccumulatedLoad(
          200,
          200,
          "200",
          List(FixedLoad(201, 201, "201", DenseVector(0, 1, 1, 1, 1)),
            FlexibleLoad(251, 251, "151", 0, DenseVector(1)),
            FlexibleLoad(251, 252, "252", 0, DenseVector(2)))
        ))

      When(
        "Allocating user with UserRepresentationAsAmplitudeInMinTimeSpan and UserRepresentationAsAmplitudeInMaxTimeSpan as heuristic")

      val userRepresentationAsAmplitude = new UserRepresentationAsAmplitudeInMinTimeSpan(
        Some(MaxPeakGraterThanMaxFixedLoadsPeakCondition, new UserRepresentationAsAmplitudeInMaxTimeSpan()))

      val usersPreferredSlots =
        UserAllocator.allocate(usersSimulation, userRepresentationAsAmplitude = userRepresentationAsAmplitude)

      Then("the flexible loads should be spread in time")

      usersPreferredSlots should contain(List(0, 1))

    }

    scenario(
      "User with two flexible loads spanning 1 time-slot, should not be spread in time when min and max user representations are provided") {

      implicit val amplitudePerSlotMetadata: DataTypeMetadata =
        DataTypeMetadata.generateDataTypeMetadata(forColumns = 5)

      Given("A user with one fixed load and two flexible loads")

      val usersSimulation: List[AccumulatedLoad] = List(
        AccumulatedLoad(
          100,
          100,
          "100",
          List(FixedLoad(101, 101, "101", DenseVector(0, 2, 2, 2, 1)),
            FlexibleLoad(151, 151, "151", 0, DenseVector(1)),
            FlexibleLoad(151, 152, "152", 0, DenseVector(1)))
        ))

      When(
        "Allocating user with UserRepresentationAsAmplitudeInMinTimeSpan and UserRepresentationAsAmplitudeInMaxTimeSpan as heuristic")

      val userRepresentationAsAmplitude = new UserRepresentationAsAmplitudeInMinTimeSpan(
        Some(MaxPeakGraterThanMaxFixedLoadsPeakCondition, new UserRepresentationAsAmplitudeInMaxTimeSpan()))

      val usersPreferredSlots =
        UserAllocator.allocate(usersSimulation, userRepresentationAsAmplitude = userRepresentationAsAmplitude)

      Then("the flexible loads should be spread in time")

      usersPreferredSlots should contain(List(0))

    }

    scenario("Users are spread in time using feedback") {

      implicit val amplitudePerSlotMetadata: DataTypeMetadata = DataTypeMetadata.generateDataTypeMetadata(forColumns = 5)

      val usersSimulation: List[AccumulatedLoad] = List(
        AccumulatedLoad(
          100,
          100,
          "100",
          List(FixedLoad(101, 101, "101", DenseVector(1, 2, 2, 1, 1)),
            FlexibleLoad(151, 151, "151", 0, DenseVector(1)),
            FlexibleLoad(151, 152, "152", 0, DenseVector(1)))
        ),
        AccumulatedLoad(
          200,
          200,
          "200",
          List(FixedLoad(201, 201, "201", DenseVector(0, 1, 1, 1, 1)),
            FlexibleLoad(251, 251, "151", 0, DenseVector(1)),
            FlexibleLoad(251, 252, "252", 0, DenseVector(1)))
        ))

      val userRepresentationAsAmplitude = new UserRepresentationAsAmplitudeInMinTimeSpan(
        Some(MaxPeakGraterThanMaxFixedLoadsPeakCondition, new UserRepresentationAsAmplitudeInMaxTimeSpan()))

      val usersPreferredSlots =
        UserAllocator.allocate(usersSimulation, userRepresentationAsAmplitude = userRepresentationAsAmplitude)

      usersPreferredSlots should contain(List(3, 4))

      println(usersPreferredSlots)

    }

    scenario("Users are spread in time using feedback, FAILING") {

      implicit val amplitudePerSlotMetadata: DataTypeMetadata = DataTypeMetadata.generateDataTypeMetadata(forColumns = 5)

      val usersSimulation: List[AccumulatedLoad] = List(
        AccumulatedLoad(500,
          500,
          "500",
          List(
            FixedLoad(101, 101, "101", DenseVector(4, 0, 4)),
            FlexibleLoad(151, 151, "151", 0, DenseVector(11))
          ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
        AccumulatedLoad(600,
          600,
          "600",
          List(
            FixedLoad(201, 201, "201", DenseVector(4, 0, 4)),
            FlexibleLoad(251, 251, "251", 0, DenseVector(12))
          ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3))
      )

      val userRepresentationAsAmplitude = new UserRepresentationAsAmplitudeInMinTimeSpan(
        Some(MaxPeakGraterThanMaxFixedLoadsPeakCondition, new UserRepresentationAsAmplitudeInMaxTimeSpan()))

      val usersPreferredSlots =
        UserAllocator.allocate(usersSimulation, userRepresentationAsAmplitude = userRepresentationAsAmplitude)

      usersPreferredSlots should contain allOf (List(1), List(0))

      println(usersPreferredSlots)

    }

  }
}

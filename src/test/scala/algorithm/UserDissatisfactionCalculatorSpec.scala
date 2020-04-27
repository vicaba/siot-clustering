package algorithm

import algorithm.scheduler.UserDissatisfactionCalculator
import breeze.linalg.DenseVector
import config.GlobalConfig
import org.scalatest.{FeatureSpec, GivenWhenThen}
import scheduler_model.load.{AccumulatedLoad, FixedLoad, FlexibleLoad}

class UserDissatisfactionCalculatorSpec extends FeatureSpec with GivenWhenThen {

  val accumulatedLoad1: AccumulatedLoad =
    AccumulatedLoad.AutoSpanFromLoads(100,
                                      100,
                                      "100",
                                      List(
                                        FixedLoad(101, 101, "101", DenseVector(0, 0, 3, 3)),
                                        FlexibleLoad(151, 151, "151", 0, DenseVector(0, 0, 3, 3))
                                      ))
  val accumulatedLoad2: AccumulatedLoad =
    AccumulatedLoad.AutoSpanFromLoads(100,
                                      100,
                                      "100",
                                      List(
                                        FixedLoad(101, 101, "101", DenseVector(0, 0, 3, 3)),
                                        FlexibleLoad(151, 151, "151", 0, DenseVector(3, 3, 0, 0))
                                      ))

  val accumulatedLoad3: AccumulatedLoad =
    AccumulatedLoad.AutoSpanFromLoads(200,
                                      200,
                                      "200",
                                      List(
                                        FixedLoad(201, 201, "201", DenseVector(3, 1, 0, 0, 2, 1)),
                                        FlexibleLoad(251, 251, "251", 0, DenseVector(0, 5, 3, 0, 0, 0))
                                      ))

  val accumulatedLoad4: AccumulatedLoad =
    AccumulatedLoad.AutoSpanFromLoads(200,
                                      200,
                                      "200",
                                      List(
                                        FixedLoad(201, 201, "201", DenseVector(3, 1, 0, 0, 2, 1)),
                                        FlexibleLoad(251, 251, "251", 0, DenseVector(0, 0, 0, 5, 3, 0))
                                      ))

  AccumulatedLoad.Mutate
    .splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(accumulatedLoad1,
                                                                GlobalConfig.instance.sequenceSplitStrategy)

  AccumulatedLoad.Mutate
    .splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(accumulatedLoad2,
                                                                GlobalConfig.instance.sequenceSplitStrategy)
  feature("User dissatisfaction is properly computed with flexible loads") {

    scenario("User dissatisfaction should be 0 if loads are not moved in position") {
      val dissatisfaction =
        UserDissatisfactionCalculator.flexibleLoadDissatisfaction(
          accumulatedLoad1.flexibleLoads.toList,
          accumulatedLoad1.flexibleLoads.toList
        )

      assert(dissatisfaction == 0, "Dissatisfaction is not 0")
    }

    scenario("User dissatisfaction should be greater than 0 and equal to 2 if loads are moved in position") {
      val dissatisfaction =
        UserDissatisfactionCalculator.flexibleLoadDissatisfaction(
          accumulatedLoad1.flexibleLoads.toList,
          accumulatedLoad2.flexibleLoads.toList
        )

      assert(dissatisfaction > 0, "Dissatisfaction is not greater than 0")
      assert(dissatisfaction == 2, "Dissatisfaction is not equal to 2")
    }

  }

  feature("User dissatisfaction is properly computed with accumulated loads") {

    scenario("User dissatisfaction should be 0 if loads are not moved in position") {
      val dissatisfaction =
        UserDissatisfactionCalculator.accumulatedLoadsDissatisfaction(
          accumulatedLoad1,
          accumulatedLoad1
        )

      assert(dissatisfaction == 0, "Dissatisfaction is not 0")
    }

    scenario("User dissatisfaction should be greater than 0 and equal to 2 if loads are moved in position") {
      val dissatisfaction =
        UserDissatisfactionCalculator.accumulatedLoadsDissatisfaction(
          accumulatedLoad1,
          accumulatedLoad2
        )

      assert(dissatisfaction > 0, "Dissatisfaction is not greater than 0")
      assert(dissatisfaction == 2, "Dissatisfaction is not equal to 2")
    }

  }

  feature("User dissatisfaction is properly computed with lists of accumulated loads") {

    scenario("User dissatisfaction should be 0 if loads are not moved in position") {
      val dissatisfaction =
        UserDissatisfactionCalculator.listOfAccumulatedLoadsDissatisfaction(
          List(accumulatedLoad1, accumulatedLoad3),
          List(accumulatedLoad1, accumulatedLoad3)
        )

      assert(dissatisfaction == 0, "Dissatisfaction is not 0")
    }

    scenario("User dissatisfaction should be greater than 0 and equal to 2 if loads are moved in position") {
      val dissatisfaction =
        UserDissatisfactionCalculator.listOfAccumulatedLoadsDissatisfaction(
          List(accumulatedLoad1, accumulatedLoad3),
          List(accumulatedLoad2, accumulatedLoad4)
        )

      assert(dissatisfaction > 0, "Dissatisfaction is not greater than 0")
      assert(dissatisfaction == 2, "Dissatisfaction is not equal to 2")
    }

    scenario(
      "User dissatisfaction should be greater than 0 and equal to 2 if loads are moved in position and " +
        "accumulated loads are not sorted") {
      val dissatisfaction =
        UserDissatisfactionCalculator.listOfAccumulatedLoadsDissatisfaction(
          List(accumulatedLoad3, accumulatedLoad1),
          List(accumulatedLoad2, accumulatedLoad4)
        )

      assert(dissatisfaction > 0, "Dissatisfaction is not greater than 0")
      assert(dissatisfaction == 2, "Dissatisfaction is not equal to 2")
    }

  }

}

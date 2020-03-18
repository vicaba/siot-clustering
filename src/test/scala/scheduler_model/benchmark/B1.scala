package scheduler_model.benchmark

import breeze.linalg.DenseVector
import org.scalatest.FeatureSpec
import scheduler_model.load.{AccumulatedLoad, FixedLoad, FlexibleLoad}
import scheduler_model.benchmark.BenchmarkHelper._
import scheduler_model.scheduler.metric_transformer.BiasedAverageDistanceTransformation

class B1 extends FeatureSpec {

  val metricTransformation = new BiasedAverageDistanceTransformation()

  scenario("6 slots with some low loads in between, 3 users") {
    val scenarioName = "6 slots with some low loads in between, 3 users"

    val users: List[AccumulatedLoad] = List(
      AccumulatedLoad.AutoSpanFromLoads(100,
        100,
        "100",
        List(
          FixedLoad(101, 101, "101", DenseVector(3, 3, 3, 0, 1, 2)),
          FlexibleLoad(151, 151, "151", 0, DenseVector(2, 3))
        )),
      AccumulatedLoad.AutoSpanFromLoads(200,
        200,
        "200",
        List(
          FixedLoad(201, 201, "201", DenseVector(3, 1, 0, 5, 2, 1)),
          FlexibleLoad(251, 251, "251", 0, DenseVector(5, 3))
        )),
      AccumulatedLoad.AutoSpanFromLoads(300,
        300,
        "300",
        List(
          FixedLoad(301, 301, "301", DenseVector(2, 0, 4, 5, 3, 1)),
          FlexibleLoad(351, 351, "351", 0, DenseVector(4, 4))
        ))
    )

    val expectedTotalLoad: List[DenseVector[Double]] = List(
      DenseVector[Double](10, 12, 10, 10, 10, 8),
      DenseVector[Double](8, 9, 10, 10, 12, 11)
    )

    executeScenario(
      users,
      expectedTotalLoad,
      metricTransformation,
      enableTestVerbose = DefaultConfiguration.enableTestVerbose,
      enableSchedulerVerbose = DefaultConfiguration.enableSchedulerVerbose,
      enablePrintLoads = DefaultConfiguration.enablePrintLoads,
      enableGenerateTables = DefaultConfiguration.enableGenerateTables,
      scenarioName = scenarioName
    )
  }
}

package scheduler_model.benchmark

import breeze.linalg.DenseVector
import org.scalatest.FeatureSpec
import scheduler_model.benchmark.BenchmarkHelper._
import scheduler_model.load.{AccumulatedLoad, FixedLoad, FlexibleLoad}
import scheduler_model.scheduler.metric_transformer.BiasedAverageDistanceTransformation
import types.clusterer.DataTypeMetadata

class B2 extends FeatureSpec {

  val metricTransformation = new BiasedAverageDistanceTransformation()

  scenario("With a gap, 2 users with 1 flexible load, the highest to the middle the other to one of the sides") {
    val scenarioName =
      "With a gap, 2 users with 1 flexible load, the highest to the middle the other to one of the sides"

    val users: List[AccumulatedLoad] = List(
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

    val expectedTotalLoad: List[DenseVector[Double]] = List(
      DenseVector[Double](19, 12, 8),
      DenseVector[Double](8, 12, 19)
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

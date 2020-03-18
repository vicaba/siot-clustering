package scheduler_model.benchmark

import breeze.linalg.DenseVector
import org.scalatest.FeatureSpec
import scheduler_model.BenchmarkS._
import scheduler_model.load.{AccumulatedLoad, FixedLoad, FlexibleLoad}
import scheduler_model.scheduler.metric_transformer.BiasedAverageDistanceTransformation
import types.clusterer.DataTypeMetadata

class B3 extends FeatureSpec {

  val metricTransformation = new BiasedAverageDistanceTransformation()

  scenario("With a gap, 2 users with 1 flexible load each to the middle") {
    val scenarioName = "With a gap, 2 users with 1 flexible load each to the middle"
    val users: List[AccumulatedLoad] = List(
      AccumulatedLoad(100,
        100,
        "100",
        List(
          FixedLoad(101, 101, "101", DenseVector(4, 0, 4)),
          FlexibleLoad(151, 151, "151", 0, DenseVector(3))
        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
      AccumulatedLoad(200,
        200,
        "200",
        List(
          FixedLoad(201, 201, "201", DenseVector(4, 0, 4)),
          FlexibleLoad(251, 251, "251", 0, DenseVector(3))
        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3))
    )

    val expectedTotalLoad: List[DenseVector[Double]] = List(
      DenseVector[Double](8, 6, 8)
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

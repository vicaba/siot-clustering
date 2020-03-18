package scheduler_model.benchmark

import breeze.linalg.DenseVector
import org.scalatest.FeatureSpec
import scheduler_model.benchmark.BenchmarkHelper._
import scheduler_model.load.{AccumulatedLoad, FixedLoad, FlexibleLoad}
import scheduler_model.scheduler.metric_transformer.BiasedAverageDistanceTransformation
import types.clusterer.DataTypeMetadata

class B5 extends FeatureSpec {

  val metricTransformation = new BiasedAverageDistanceTransformation()

  scenario("3 slots with 3 users and a flexible load each") {
    val scenarioName = "3 slots with 3 users and a flexible load each"
    val users: List[AccumulatedLoad] = List(
      AccumulatedLoad(100,
        100,
        "100",
        List(
          FixedLoad(101, 101, "101", DenseVector(2, 4, 1)),
          FlexibleLoad(151, 151, "151", 0, DenseVector(3))
        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
      AccumulatedLoad(200,
        200,
        "200",
        List(
          FixedLoad(201, 201, "201", DenseVector(2, 4, 1)),
          FlexibleLoad(251, 251, "251", 0, DenseVector(3))
        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
      AccumulatedLoad(300,
        300,
        "300",
        List(
          FixedLoad(301, 301, "301", DenseVector(2, 4, 1)),
          FlexibleLoad(351, 351, "351", 0, DenseVector(3))
        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3))
    )

    val expectedTotalLoad: List[DenseVector[Double]] = List(
      DenseVector[Double](12, 12, 6),
      DenseVector[Double](6, 12, 12),
      DenseVector[Double](9, 12, 9)
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

package scheduler_model.benchmark

import breeze.linalg.DenseVector
import org.scalatest.FeatureSpec
import scheduler_model.BenchmarkS._
import scheduler_model.load.{AccumulatedLoad, FixedLoad, FlexibleLoad}
import scheduler_model.scheduler.metric_transformer.BiasedAverageDistanceTransformation
import types.clusterer.DataTypeMetadata

class B4 extends FeatureSpec {

  val metricTransformation = new BiasedAverageDistanceTransformation()

  scenario("All users have the same loads") {
    val scenarioName = "All users have the same loads"

    val users: List[AccumulatedLoad] = List(
      AccumulatedLoad(100,
        100,
        "100",
        List(
          FixedLoad(101, 101, "101", DenseVector(1, 1, 1)),
          FlexibleLoad(151, 151, "151", 0, DenseVector(1, 1, 1))
        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
      AccumulatedLoad(200,
        200,
        "200",
        List(
          FixedLoad(201, 201, "201", DenseVector(1, 1, 1)),
          FlexibleLoad(251, 251, "251", 0, DenseVector(1, 1, 1))
        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3))
    )

    val expectedTotalLoad: List[DenseVector[Double]] = List(
      DenseVector[Double](4, 4, 4)
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

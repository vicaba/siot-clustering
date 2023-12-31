package algorithm.algorithm_scheduler

import algorithm.BatchRunSettingsBuilder
import batch.GenBatchRun
import breeze.linalg.{DenseMatrix, sum}
import metrics.{Metric, Par}
import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._
import reader.TemplateForSyntheticProfilesReader
import types.clusterer.DataTypeMetadata
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster

class AlgorithmSpec extends FlatSpec with GivenWhenThen {

  Given("A user with two loads")

  val rawUnscheduledLoads: Point = Point(
    1,
    DenseMatrix(
      (4.0, 4.0, 4.0, 3.0, 3.0),
      (1.0, 1.0, 0.0, 0.0, 0.0)
    ),
    List(TemplateForSyntheticProfilesReader.Appliances.EInst, TemplateForSyntheticProfilesReader.Appliances.DishWasher)
  )(DataTypeMetadata.generateDataTypeMetadata(forColumns = 5))

  And("Algorithm settings")

  val testBatchRunSettingsBuilder =
    new BatchRunSettingsBuilder(Vector(rawUnscheduledLoads), List(1), List(Par.withParAggregate), (_, _) => 1)

  When("clustered and rescheduled")

  val stepsList = GenBatchRun(testBatchRunSettingsBuilder.build)

  val unscheduledLoads: List[Cluster] = stepsList.head.clustererOutput.clusters

  val scheduledLoads: List[Cluster] = stepsList.head.reschedulerOutput.clusters

  Then("ScheduledLoads PAR is lower or equal than UnscheduledLoads PAR.")

  val unscheduledLoadsPar: Double = Metric.par(unscheduledLoads)
  val scheduledLoadsPar: Double   = Metric.par(scheduledLoads)

  scheduledLoadsPar should be < unscheduledLoadsPar

  info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
  info(s"PAR for scheduled loads: $scheduledLoadsPar.")

  And("scheduledLoads total energy is equal to unscheduledLoads total energy")

  val totalEnergyScheduledLoads: Double      = sum(scheduledLoads.head.syntheticValue)
  val totalEnergyUnscheduledLoads: Double    = sum(unscheduledLoads.head.syntheticValue)
  val totalEnergyRawUnscheduledLoads: Double = sum(rawUnscheduledLoads.syntheticValue)

  totalEnergyUnscheduledLoads shouldBe totalEnergyRawUnscheduledLoads

  totalEnergyScheduledLoads shouldBe totalEnergyUnscheduledLoads

  totalEnergyScheduledLoads shouldBe totalEnergyRawUnscheduledLoads

}

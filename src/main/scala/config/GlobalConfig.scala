package config

import config.GlobalConfig.{ClustererType, SchedulerType}
import reader.{EgaugeFlexibleLoadsKeys, FlexibleLoadsKeys, SyntheticLoadsFlexibleLoadsKeys}
import scheduler_model.sequence_split.{SequenceSplitByConsecutiveElements, SequenceSplitByZero, SequenceSplitStrategy}

trait GlobalConfig {

  val flexibleLoadsKeys: List[String]

  val sequenceSplitStrategy: SequenceSplitStrategy

  val clustererType: ClustererType

  val schedulerType: SchedulerType

}

object GlobalConfig {

  trait ClustererType

  object ClustererType {
    object Random    extends ClustererType
    object Euclidean extends ClustererType
  }

  trait SchedulerType

  object SchedulerType {
    object Coordinated   extends SchedulerType
    object UnCoordinated extends SchedulerType
  }

  /**
    * SyntheticGlobalConfig is de default global configuration for tests
    */
  var instance: GlobalConfig = SyntheticGlobalConfig()
}

case class EgaugeGlobalConfig(
    override val flexibleLoadsKeys: List[String] = EgaugeFlexibleLoadsKeys.flexibleLoads,
    override val sequenceSplitStrategy: SequenceSplitStrategy = SequenceSplitByZero,
    override val clustererType: ClustererType = GlobalConfig.ClustererType.Euclidean,
    override val schedulerType: SchedulerType = GlobalConfig.SchedulerType.Coordinated
) extends GlobalConfig

case class SyntheticGlobalConfig(
    override val flexibleLoadsKeys: List[String] = SyntheticLoadsFlexibleLoadsKeys.flexibleLoads,
    override val sequenceSplitStrategy: SequenceSplitStrategy =
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage,
    override val clustererType: ClustererType = GlobalConfig.ClustererType.Euclidean,
    override val schedulerType: SchedulerType = GlobalConfig.SchedulerType.UnCoordinated
) extends GlobalConfig

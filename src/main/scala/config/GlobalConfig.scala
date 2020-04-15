package config

import reader.{EgaugeFlexibleLoadsKeys, FlexibleLoadsKeys, SyntheticLoadsFlexibleLoadsKeys}
import scheduler_model.sequence_split.{SequenceSplitByConsecutiveElements, SequenceSplitByZero, SequenceSplitStrategy}

trait GlobalConfig {

  val flexibleLoadsKeys: List[String]

  val sequenceSplitStrategy: SequenceSplitStrategy

}

object GlobalConfig {
  /**
    * SyntheticGlobalConfig is de default global configuration for tests
    */
  var instance: GlobalConfig = SyntheticGlobalConfig()
}

case class EgaugeGlobalConfig(
    override val flexibleLoadsKeys: List[String] = EgaugeFlexibleLoadsKeys.flexibleLoads,
    override val sequenceSplitStrategy: SequenceSplitStrategy = SequenceSplitByZero
) extends GlobalConfig

case class SyntheticGlobalConfig(
    override val flexibleLoadsKeys: List[String] = SyntheticLoadsFlexibleLoadsKeys.flexibleLoads,
    override val sequenceSplitStrategy: SequenceSplitStrategy =
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage
) extends GlobalConfig

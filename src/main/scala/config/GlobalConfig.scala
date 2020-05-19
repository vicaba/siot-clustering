package config

import config.GlobalConfig.ClustererType
import reader.{EgaugeFlexibleLoadsKeys, FlexibleLoadsKeys, SyntheticLoadsFlexibleLoadsKeys}
import scheduler_model.sequence_split.{SequenceSplitByConsecutiveElements, SequenceSplitByZero, SequenceSplitStrategy}

trait GlobalConfig {

  val flexibleLoadsKeys: List[String]

  val sequenceSplitStrategy: SequenceSplitStrategy

  val clustererType: ClustererType

}

object GlobalConfig {

  trait ClustererType

  object ClustererType {
    object Random    extends ClustererType
    object Euclidean extends ClustererType
  }

  /**
    * SyntheticGlobalConfig is de default global configuration for tests
    */
  var instance: GlobalConfig = SyntheticGlobalConfig()
}

case class EgaugeGlobalConfig(
    override val flexibleLoadsKeys: List[String] = EgaugeFlexibleLoadsKeys.flexibleLoads,
    override val sequenceSplitStrategy: SequenceSplitStrategy = SequenceSplitByZero,
    override val clustererType: ClustererType = GlobalConfig.ClustererType.Euclidean
) extends GlobalConfig

case class SyntheticGlobalConfig(
    override val flexibleLoadsKeys: List[String] = SyntheticLoadsFlexibleLoadsKeys.flexibleLoads,
    override val sequenceSplitStrategy: SequenceSplitStrategy =
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage,
    override val clustererType: ClustererType = GlobalConfig.ClustererType.Euclidean
) extends GlobalConfig



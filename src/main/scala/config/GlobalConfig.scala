package config

import reader.{EgaugeFlexibleLoadsKeys, FlexibleLoadsKeys, SyntheticLoadsFlexibleLoadsKeys}

trait GlobalConfig {

  val flexibleLoadsKeys: List[String]

}

object GlobalConfig {
  var instance: GlobalConfig = SyntheticGlobalConfig()
}

case class EgaugeGlobalConfig(
    override val flexibleLoadsKeys: List[String] = EgaugeFlexibleLoadsKeys.flexibleLoads
) extends GlobalConfig

case class SyntheticGlobalConfig(
  override val flexibleLoadsKeys: List[String] = SyntheticLoadsFlexibleLoadsKeys.flexibleLoads
) extends GlobalConfig


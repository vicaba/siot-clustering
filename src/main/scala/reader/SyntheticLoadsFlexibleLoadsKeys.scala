package reader

object SyntheticLoadsFlexibleLoadsKeys extends FlexibleLoadsKeys {

  import TemplateForSyntheticProfilesReader.Appliances._

  override val flexibleLoads: List[String] = List(
    DishWasher,
    TumbleDryer,
    WashingMachine,
    WasherDryer
  )

}

package appliances

trait Appliance {

  val name: String

  val maxConsumption: Double

  val minConsumption: Double

}

trait NonShiftableAppliance extends Appliance

trait ShiftableAppliance extends Appliance

case class FlexibleAppliance(name: String, maxConsumption: Double, minConsumption: Double) extends ShiftableAppliance

trait DeferrableAppliance extends ShiftableAppliance

case class DeferrableNonInterruptibleAppliance(name: String, maxConsumption: Double, minConsumption: Double) extends DeferrableAppliance

case class DeferrableInterruptibleAppliance(name: String, maxConsumption: Double, minConsumption: Double) extends DeferrableAppliance

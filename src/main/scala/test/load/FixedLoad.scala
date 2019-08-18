package test.load

case class FixedLoad(override val id: Int,
                     override val positionInT: Int,
                     override val amplitudePerSlot: Vector[Double],
                     override val label: String = "")
    extends SingleLoad {
  override def span: Int           = amplitudePerSlot.size
  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)
}

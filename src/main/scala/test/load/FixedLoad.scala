package test.load

case class FixedLoad(override val id: Int,
                     override val positionInT: Int,
                     override val amplitudePerSlot: Vector[Double],
                     override val label: String = "")
    extends SingleLoad

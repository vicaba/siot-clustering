package test.load

object FlexibleLoad {

  def apply(id: Int, positionInT: Int, amplitudePerSlot: Vector[Double], label: String = ""): FlexibleLoad =
    new FlexibleLoad(id, positionInT, amplitudePerSlot, label)

}

class FlexibleLoad(override val id: Int,
                   private var _positionInT: Int,
                   private val _amplitudePerSlot: Vector[Double],
                   override val label: String = "")
    extends SingleLoad {

  override def positionInT: Int = _positionInT

  def positionInT_=(pos: Int): FlexibleLoad = {
    _positionInT = pos
    this
  }

  override val span: Int = amplitudePerSlot.size

  override def amplitudePerSlot: Vector[Double] = _amplitudePerSlot

  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)

  def copy(id: Int = this.id,
           positionInT: Int = this.positionInT,
           amplitudePerSlot: Vector[Double] = this.amplitudePerSlot,
           label: String = this.label): FlexibleLoad =
    FlexibleLoad(id, positionInT, amplitudePerSlot, label)

}

package scheduler_model.load
import scheduler_model.load.Load.{GroupId, LoadId}
import breeze.linalg._

class FlexibleLoadRepresentation(override val id: LoadId,
                                 override val group: GroupId,
                                 override val label: String,
                                 val amplitude: Double,
                                 val maxTimeSpan: Int,
                                 val minTimeSpan: Int)
    extends FlexibleLoad(id, group, label, 0, DenseVector(amplitude)) {

  override def copy(): FlexibleLoadRepresentation = new FlexibleLoadRepresentation(id, group, label, amplitude, maxTimeSpan, minTimeSpan)

}

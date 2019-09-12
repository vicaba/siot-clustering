package new_test.load

import breeze.linalg.DenseVector
import new_test.load.Load._
import types.clusterer.DataTypeMetadata

object Load {

  type LoadId = Int
  type GroupId = Int

}

trait Load {

  this: Load =>

  val amplitudePerSlotMetadata: DataTypeMetadata

  def id: LoadId
  def group: GroupId
  val label: String

  def amplitudePerSlot: DenseVector[Double]
  def startPositionInTime: Int
  def span: Int = amplitudePerSlot.length

  override def toString: String = s"${getClass.getCanonicalName}($id) "

  protected def ensureCorrectCreation(): Unit = {
    assert(amplitudePerSlot.length == amplitudePerSlotMetadata.Columns)
  }

}

trait SingleLoad extends Load

trait BaseLoad extends SingleLoad {
  override def startPositionInTime: LoadId = 0
}




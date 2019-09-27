package new_test.load

import breeze.linalg._
import new_test.load.Load._
import types.clusterer.DataTypeMetadata

import scala.collection.mutable.ListBuffer

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

object LoadOps {

  def expandToCols(startPosition: Int, vector: DenseVector[Double], cols: Int): DenseVector[Double] = {
    val span = vector.length
    val baseVector = DenseVector.fill[Double](cols, 0.0)
    for ((baseVectorIndex, vectorIndex) <- (startPosition until cols) zip (0 until span))
      yield {
        baseVector(baseVectorIndex) = vector(vectorIndex)
      }
    baseVector
  }

  def aggregatedAmplitudePerSlot(loads: Iterable[Load], amplitudeInOffStatus: Double, dataTypeMetadata: DataTypeMetadata): DenseVector[Double] =
    if (loads.isEmpty) DenseVector.fill(dataTypeMetadata.Columns)(amplitudeInOffStatus)
    else {

      val aggregatedVector = sum(loads.map( l => expandToCols(l.startPositionInTime, l.amplitudePerSlot, dataTypeMetadata.Columns)))

      val restPositions = Array.fill(dataTypeMetadata.Columns)(true)

      loads.foreach { l =>
        for (idx <- l.startPositionInTime until (l.startPositionInTime + l.span)) {
          restPositions(idx) = false
        }
      }

      restPositions.zipWithIndex.foreach { case (e, idx) =>
          if (!e) aggregatedVector(idx) = amplitudeInOffStatus
      }

      aggregatedVector

    }

}




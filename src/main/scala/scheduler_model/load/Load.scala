package scheduler_model.load

import breeze.linalg._
import metrics.DenseVectorReprOps
import scheduler_model.load.Load._
import types.clusterer.DataTypeMetadata

import scala.util.Try

object Load {

  type LoadId = Int
  type GroupId = Int

  val loadOrderingByPositionInTime: Ordering[Load] =
    (x: Load, y: Load) => implicitly[Ordering[Int]].compare(x.startPositionInTime, y.startPositionInTime)

  val loadOrderingByAmplitude: Ordering[Load] =
    (x: Load, y: Load) => implicitly[Ordering[Double]].compare(x.totalEnergy, y.totalEnergy)

  val loadListOrderingByMaxPositionInT: Ordering[List[Load]] = new Ordering[List[Load]] {
    override def compare(x: List[Load], y: List[Load]): LoadId = {
      // if x or y are empty, "max" throws an exception, guard clauses
      if (x.isEmpty && y.isEmpty) return 0
      val intOrdering = implicitly[Ordering[Int]]
      if (x.isEmpty) return intOrdering.compare(0, y.map(_.startPositionInTime).max)
      if (y.isEmpty) return intOrdering.compare(x.map(_.startPositionInTime).max, 0)
      intOrdering.compare(x.map(_.startPositionInTime).max, y.map(_.startPositionInTime).max)
    }
  }

  val loadListOrderingByAmplitude: Ordering[List[Load]] =
    (x: List[Load], y: List[Load]) =>
    // Nil[Int].sum == 0
      implicitly[Ordering[Double]].compare(x.map(_.totalEnergy).sum, y.map(_.totalEnergy).sum)

  implicit def toVector[X <: Load]: DenseVectorReprOps[X] = new DenseVectorReprOps[X] {

    override def apply(t: X): DenseVector[Double] = t.amplitudePerSlot.toDenseVector

    override def zero(t: X): DenseVector[Double] = DenseVector((for (_ <- 1 to t.span) yield 0.0): _*)

  }

  implicit def toVectorTraversable[X <: Load, S[X] <: Iterable[X]]: DenseVectorReprOps[S[X]] = new DenseVectorReprOps[S[X]] {

    override def apply(t: S[X]): DenseVector[Double] = sum(t.map(_.amplitudePerSlot)).toDenseVector

    override def zero(t: S[X]): DenseVector[Double] = DenseVector((for (_ <- 1 to LoadOps.span(t)) yield 0.0): _*)

  }

}

trait Load {

  def amplitudePerSlotMetadata: DataTypeMetadata

  def id: LoadId

  def group: GroupId

  val label: String

  def amplitudePerSlot: Vector[Double]

  def startPositionInTime: Int

  def peak: Double = max(amplitudePerSlot)

  def span: Int = amplitudePerSlot.length

  def totalEnergy: Double = sum(amplitudePerSlot)

  override def equals(obj: Any): Boolean = obj match {
    case s: Load => this.getClass == s.getClass && s.id == this.id && s.group == this.group && s.label == this.label
    case _ => false
  }

  override def hashCode(): Int = this.id

  protected def ensureCorrectCreation(): Unit = {
    val length = amplitudePerSlot.length
    val columns = amplitudePerSlotMetadata.Columns
    assert(
      length == columns,
      "amplitudePerSlot.length was " + length + ", while metadata.columns were " + columns
    )
  }

  override def toString: String = s"Load($id, $startPositionInTime, $amplitudePerSlot)"

}

trait SingleLoad extends Load

object LoadOps {

  /**
    * Expands vector that start at startPosition to a vector from 0 to cols where vector is compressed in range [0, cols)
    * @param startPosition
    * @param vector
    * @param cols
    * @return
    */
  def expandToCols(startPosition: Int, vector: Vector[Double], cols: Int): Vector[Double] = {
    if(vector.length == cols) return vector
    val span = vector.length
    val baseVector = DenseVector.fill[Double](cols, 0.0)
    for ((baseVectorIndex, vectorIndex) <- (startPosition until cols) zip (0 until span))
      yield {
        baseVector(baseVectorIndex) = vector(vectorIndex)
      }
    baseVector
  }

  def expandToCols(load: Load, cols: Int): Vector[Double] =
    expandToCols(load.startPositionInTime, load.amplitudePerSlot, cols)

  def aggregatedAmplitudePerSlot(loads: Iterable[Load], amplitudeInOffStatus: Double, dataTypeMetadata: DataTypeMetadata): Vector[Double] =
    if (loads.isEmpty) DenseVector.fill(dataTypeMetadata.Columns)(amplitudeInOffStatus).toVector
    else {

      val aggregatedVector = sum(loads.map(l => expandToCols(l, dataTypeMetadata.Columns)))

      val restPositions = Array.fill(dataTypeMetadata.Columns)(true)

      loads.foreach { l =>
        for (idx <- l.startPositionInTime until (l.startPositionInTime + l.span)) {
          restPositions(idx) = false
        }
      }

      restPositions.zipWithIndex.foreach { case (e, idx) =>
        if (e) aggregatedVector(idx) = amplitudeInOffStatus
      }

      aggregatedVector

    }

  def areLoadsOverlapped(loads: Iterable[Load]): Boolean = {
    val span = LoadOps.span(loads)
    val minPositionIntT = loads.map(_.startPositionInTime).min
    val restPositions = Array.fill(span)(false)
    loads.foreach { l =>
      for (idx <- (l.startPositionInTime - minPositionIntT) until (l.startPositionInTime + l.span - minPositionIntT)) {
        if (restPositions(idx)) return true
        restPositions(idx) = true
      }
    }
    false
  }

  def span(loads: Iterable[Load]): Int =
    Try(loads.map(l => l.span + l.startPositionInTime).max - loads.map(_.startPositionInTime).min).getOrElse(0)

  def copy(loads: Iterable[Load]): Iterable[Load] =
    loads.filter(!_.isInstanceOf[FlexibleLoadSubTask]).flatMap(copyOne(_, addSuperTaskSubTasks = false))

  def copy(loads: Iterable[Load], addSuperTaskSubTasks: Boolean): Iterable[Load] =
    loads.filter(!_.isInstanceOf[FlexibleLoadSubTask]).flatMap(copyOne(_, addSuperTaskSubTasks))

  def copy(load: AccumulatedLoad): AccumulatedLoad =
    load.copy(addSuperTaskSubTasks = false)

  def copy(load: AccumulatedLoad, addSuperTaskSubTasks: Boolean): AccumulatedLoad =
    load.copy(addSuperTaskSubTasks)

  def copy(load: FlexibleLoadSuperTask): FlexibleLoadSuperTask =
    load.copyAndCopySubTasks()

  private def copyOne(l: Load, addSuperTaskSubTasks: Boolean): Iterable[Load] = l match {
    case fixedLoad: FixedLoad => List(fixedLoad.copy())
    case flexibleLoadSuperTask: FlexibleLoadSuperTask =>
      val superTask = flexibleLoadSuperTask.copyAndCopySubTasks()
      val subTasks = superTask.aggregatees
      if (addSuperTaskSubTasks) List(superTask) ++ subTasks else List(superTask)
    case _: FlexibleLoadSubTask => Nil
    case flexibleLoad: FlexibleLoad => List(flexibleLoad.copy())
    case accumulatedLoad: AccumulatedLoad => List(accumulatedLoad.copy(addSuperTaskSubTasks))
  }

}




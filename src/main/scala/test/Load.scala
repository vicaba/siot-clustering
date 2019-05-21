package test

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

sealed trait Load {
  def positionInT: Int
  def amplitude: Double
}

sealed trait SingleLoad extends Load {
  def id: Int
}

case class FixedLoad(override val id: Int, override val positionInT: Int, override val amplitude: Double)
  extends SingleLoad {
  override def toString: String = s"FixedLoad($id, $positionInT, $amplitude)"
}

case class FlexibleLoad(override val id: Int, override val positionInT: Int, override val amplitude: Double)
  extends SingleLoad {
  override def toString: String = s"FlexibleLoad($id, $positionInT, $amplitude)"
}

case class AccumulatedLoad(override val positionInT: Int, loads: List[SingleLoad]) extends Load {
  def amplitude: Double = loads.foldLeft(0.0)((accum, load) => accum + load.amplitude)
}

class Loads(val fixedLoads: Vector[FixedLoad], val flexibleLoads: Vector[FlexibleLoad])

object Load {

  def toFixedLoads[S[X] <: Seq[X]](l: S[Double])(implicit cbf: CanBuildFrom[Nothing, FixedLoad, S[FixedLoad]]): S[FixedLoad] = {
    l.zipWithIndex.map { case (e, idx) => new FixedLoad(idx, idx, e) }.to[S]
  }

  def toFlexibleLoads[S[X] <: Seq[X]](l: S[Double])(implicit cbf: CanBuildFrom[Nothing, FlexibleLoad, S[FlexibleLoad]]): S[FlexibleLoad] = {
    l.zipWithIndex.map { case (e, idx) => new FlexibleLoad(idx, idx, e) }.to[S]
  }

  class LoadOrdering extends Ordering[Load] {
    override def compare(x: Load, y: Load): Int = implicitly[Ordering[Double]].compare(x.amplitude, y.amplitude)
  }

  implicit val loadOrdering: LoadOrdering = new LoadOrdering

  implicit def toVector[X <: Load]: DenseVectorReprOps[Vector[X]] = new DenseVectorReprOps[Vector[X]] {

    override def apply(t: Vector[X]): DenseVector[Double] = DenseVector(t.map(_.amplitude):_*)

    override def zero(t: Vector[X]): DenseVector[Double] = DenseVector((for(_ <- 1 to t.size) yield 0.0):_*)
  }

}


/*
package test

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait Load {
  def positionInT: Int
  def amplitude: Double
}

trait SingleLoad extends Load {
  def id: Int
}

class FixedLoad(override val id: Int, override val positionInT: Int, override val amplitude: Double)
  extends SingleLoad {
  override def toString: String = s"FixedLoad($id, $positionInT, $amplitude)"
}

class FlexibleLoad(override val id: Int, override val positionInT: Int, override val amplitude: Double)
  extends SingleLoad {
  override def toString: String = s"FlexibleLoad($id, $positionInT, $amplitude)"
}

class AccumulatedLoad(override val positionInT: Int, val loads: List[SingleLoad]) extends Load {
  def amplitude: Double = loads.foldLeft(0.0)((accum, load) => accum + load.amplitude)
}

class Loads(val fixedLoads: Vector[FixedLoad], val flexibleLoads: Vector[FlexibleLoad])

object Load {

  implicit def toFixedLoads[S[X] <: Seq[Double]](l: S)(implicit cbf: CanBuildFrom[Nothing, FixedLoad, S[FixedLoad]]): S[FixedLoad] = {
    l.map(_ => new FixedLoad(1, 1, 1)).to[S[FixedLoad]]
  }

}
*/

package test

import scala.collection.generic.CanBuildFrom

class FixedLoad(val id: Int, val positionInT: Int, val amplitude: Double) {
  override def toString: String = s"FixedLoad($id, $positionInT, $amplitude)"
}

object Load {

  implicit def toFixedLoads[S[_] <: Seq[Double]](l: S[Double])(implicit cbf: CanBuildFrom[Nothing, FixedLoad, S[FixedLoad]]): S[FixedLoad] = {
    l.map(_ => new FixedLoad(1, 1, 1)).to[S]
  }

  def main(args: Array[String]): Unit = {
    println(toFixedLoads(List(1.0, 2.0, 3.0)))
  }

}

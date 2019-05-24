package test

import scala.collection.generic.CanBuildFrom

object SeqOps {

  def sum[X: Numeric, S[Y] <: Seq[Y]](s: Seq[S[X]])
    (implicit cbf: CanBuildFrom[Nothing, X, S[X]]): Seq[X] =

  /*
      This code compiles
       s.reduce{(itrA, itrB) =>
          val num = implicitly[Numeric[X]]
          val sum = new num.Ops(itrA.head).+(itrB.head)
          itrA
          */

      s.reduce{(itrA, itrB) =>
        itrA.zip(itrB).map { // it seems that itrB loses the type here :/
          case (a, b) =>
            val num = implicitly[Numeric[X]]
            val sum = new num.Ops(a).+(b)
            sum
      }
  }

  def main(args: Array[String]): Unit = {
    sum(Seq(Vector(1), Vector(1)))
  }

}

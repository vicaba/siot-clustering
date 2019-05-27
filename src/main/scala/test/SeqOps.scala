package test

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.collection.SeqLike

object SeqOps {


  def sum[X: Numeric, S[Y] <: Seq[Y] with SeqLike[Y, S[Y]]]
  (s: Seq[S[X]])
  (implicit
  cbf1: CanBuildFrom[S[_], X, S[X]],
  cbf2: CanBuildFrom[S[_], (X, X), S[(X, X)]]
  )
  : S[X] = {
    val num = implicitly[Numeric[X]]
    import num._
    s.reduce(_.zip(_)(cbf2).map{ case (a, b) => a + b })
  }

  def main(args: Array[String]): Unit = {
    println(sum(Seq(Vector(1), Vector(1))))     // Vector(2)
    println(sum(Seq(List(2, 3), List(4, 5))))   // List(6, 8)
  }

}
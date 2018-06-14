package main


import breeze.linalg.DenseVector
import cluster.Algorithm
import cluster.Types._
import reader.Reader

object Main {

  def main(args: Array[String]): Unit = {
    val algorithmBuilder = new Algorithm()
    val points = Reader.readUserRanges().zipWithIndex.map { case (values, idx) =>
      val v = EmptyData()
      v(0, ::) := DenseVector[Int](values: _*).t
      Point(idx, v)
    }
    val c = algorithmBuilder.run(3, points)

  }

}

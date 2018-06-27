package main


import breeze.linalg.DenseVector
import clustering.Algorithm
import types._
import types.Types._
import config.Configuration
import metrics.Metric
import reader.Reader
import types.Point

object Main {

  def main(args: Array[String]): Unit = {
    val algorithmBuilder = Algorithm
    val points = Reader.readUserRanges(Configuration.userProfilesFile).zipWithIndex.map { case (values, idx) =>
      val v = EmptyData()
      v(0, ::) := DenseVector[Double](values: _*).t
      Point(idx, v)
    }
    val c = algorithmBuilder.run(3, points, Metric.par, 1.0)

  }

}

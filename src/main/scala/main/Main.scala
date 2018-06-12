package main

import cluster.{AlgorithmElementsBuilder, Point}
import reader.Reader

object Main {

  def main(args: Array[String]): Unit = {
    val algorithmBuilder = new AlgorithmElementsBuilder[Int]
    val toSingleValue = (values: algorithmBuilder.PointValueType) => 2
    val points = Reader.readUserRanges().map { values =>
      algorithmBuilder.createPoint(Vector(values), toSingleValue)
    }
    algorithmBuilder.run(3, points)

    println(points)

  }

}

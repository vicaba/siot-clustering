package reader

import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVRecord
import java.io.FileReader
import java.io.Reader
import scala.collection.JavaConverters._


import cluster.Point


object Reader {

  def main(args: Array[String]): Unit = {
    println(readUserRanges())
  }

  def readUserRanges(): Vector[Vector[Int]] = {

    val in = new FileReader("./src/main/resources/UserRanges.csv")
    val records = CSVFormat.DEFAULT.parse(in)
    records.getRecords.asScala.map { c =>
      (for (i <- 0 until c.size()) yield {
        c.get(i).toInt
      }).toVector
    }.toVector
  }

}

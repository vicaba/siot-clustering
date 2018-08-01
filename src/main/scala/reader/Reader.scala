package reader

import org.apache.commons.csv.CSVFormat
import java.io.FileReader

import config.Configuration

import scala.collection.JavaConverters._

object Reader {

  def readUserRanges(file: String): Vector[Vector[Double]] = {
    val in      = new FileReader(file)
    val records = CSVFormat.DEFAULT.parse(in)
    records.getRecords.asScala.map { c =>
      (for (i <- 0 until c.size()) yield {
        c.get(i).toDouble
      }).toVector
    }.toVector
  }

}

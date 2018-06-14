package reader

import org.apache.commons.csv.CSVFormat
import java.io.FileReader

import scala.collection.JavaConverters._


object Reader {

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

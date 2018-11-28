package reader

import org.apache.commons.csv.CSVFormat
import java.io.{FileInputStream, FileReader}

import breeze.linalg.{DenseMatrix, DenseVector}
import config.Configuration
import play.api.libs.json.{JsValue, Json}
import types.Types67_24
import types.immutable.Point

import scala.collection.JavaConverters._
import scala.util.Try

object Reader {

  def readEgaugeData(file: String): Vector[Point] = {

    val stream = new FileInputStream(file)
    Try(Json.parse(stream)).fold(
      _ => {
        stream.close()
        Vector.empty[Point]
      },
      jsval => {
        stream.close()
        jsval
          .validate[List[JsValue]]
          .fold(
            _ => Vector.empty[Point],
            jsList => {
              jsList.map { jsPoint =>
                val dataid = (jsPoint \ "dataid").validate[Int].get
                val vectorList = (jsPoint \ "data").validate[List[List[Double]]].get.map { applianceList =>
                  val vector = DenseVector(applianceList: _*)
                  if (vector.length == 0) {
                    vector
                  } else {
                    vector
                  }
                }
                val data = DenseMatrix(vectorList: _*)

                val point = Point(dataid, data, None)(Types67_24)

                point
              }.toVector
            }
          )
      }
    )

  }

}

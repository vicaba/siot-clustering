package reader

import java.io.FileInputStream

import breeze.linalg.{DenseMatrix, DenseVector}
import play.api.libs.json.{JsValue, Json}
import types.Types67_24
import types.immutable.Point

import scala.util.Try

object EgaugeReader {

  def apply(file: String): Vector[Point] = {

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
                  DenseVector(applianceList: _*)
                }
                val data = DenseMatrix(vectorList: _*)

                val point = Point(dataid, data, Nil, None)(Types67_24)

                point
              }.toVector
            }
          )
      }
    )

  }

}

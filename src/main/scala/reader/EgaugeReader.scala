package reader

import java.io.FileInputStream

import breeze.linalg.{DenseMatrix, DenseVector}
import play.api.libs.json.{JsValue, Json}
import types.clusterer.Types67_24
import types.clusterer.immutable.Point

import scala.util.Try

object EgaugeReader {

  val appliances = "winecooler1,pool1,lights_plugs4,icemaker1,bedroom1,bathroom1,freezer1,furnace1,livingroom1,air1,security1,refrigerator1,lights_plugs3,oven2,lights_plugs5,garage1,range1,bedroom2,waterheater2,bathroom2,air3,kitchen1,disposal1,office1,car1,venthood1,diningroom2,lights_plugs2,bedroom5,aquarium1,outsidelights_plugs1,outsidelights_plugs2,poolpump1,grid,oven1,clotheswasher1,waterheater1,kitchen2,lights_plugs6,bedroom3,dryg1,drye1,refrigerator2,kitchenapp1,pool2,lights_plugs1,utilityroom1,clotheswasher_dryg1,dishwasher1,heater1,use,diningroom1,airwindowunit1,poollight1,furnace2,livingroom2,microwave1,sprinkler1,kitchenapp2,housefan1,jacuzzi1,bedroom4,shed1,air2,garage2,gen,pump1".split(",").toList

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

                val point = Point(dataid, data, appliances, None)(Types67_24)

                point
              }.toVector
            }
          )
      }
    )

  }

}

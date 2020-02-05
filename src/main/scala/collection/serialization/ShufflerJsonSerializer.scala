package collection.serialization

import collection.shuffler.{Keep, Random, Shuffler}
import play.api.libs.json.{JsString, Writes}

object ShufflerJsonSerializer {

  val shufflerWrites: Writes[Shuffler] = (o: Shuffler) => JsString {
    o match {
      case _: Keep.type => "keep"
      case _: Random => "random"
    }
  }

}

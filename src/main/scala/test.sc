import utils.MathUtils

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

val factors1 = MathUtils.factorize(3)
val mset1 = factors1.groupBy(identity) map { case (c, cs) => (c, cs.size) }

val factors2 = MathUtils.factorize(21)
val mset2 = factors2.groupBy(identity) map { case (c, cs) => (c, cs.size) }

val builderMap = scala.collection.mutable.Map[Int, Int]()

mset1.keys.foreach { e =>
  mset2.keys.find(_ == e).fold(Unit){ e2 =>
    println(e2)
    builderMap.+=(e2 -> (mset1(e) - mset2(e2)))
    Unit
  }
}

builderMap

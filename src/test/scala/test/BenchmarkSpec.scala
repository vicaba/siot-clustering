package test

import org.scalatest.{FeatureSpec, GivenWhenThen}
import Load._
import metrics.Metric

class BenchmarkSpec extends FeatureSpec with GivenWhenThen {
  feature("Benchmark between the input PAR and the output PAR of the two alternatives (with and without prefered slots per user)") {
    scenario("Simple case") {
      val users = getUsers

      println(s"IN total PAR = ${Metric.par(SpanSlotAccumulatedLoad(-1, 0, users))}")
      val maxNumOfSlots = users.flatMap(_.loads.map(_.amplitudePerSlot.size)).max
      val usersPreferedSlots = UserAllocator.allocate(users = users, numOfSlots = maxNumOfSlots, slotsWindowSize = 1)

      var resultsWithoutPreferedSlots: List[SpanSlotAccumulatedLoad] = List()
      var resultsWithPreferedSlots: List[SpanSlotAccumulatedLoad] = List()

      for (i <- users.indices) {
        val user = users(i)
        val userPreferedSlots = usersPreferedSlots(i)

        println(s"\tUser ${user.id}")
        println(s"\t\tIN PAR = ${Metric.par(user)}")
        println(s"\t\tPrefred slots -> ${userPreferedSlots.toString()}")

        val resultWithoutPreferedSlots = Rescheduler.reschedule(user)
        resultsWithoutPreferedSlots = resultWithoutPreferedSlots :: resultsWithoutPreferedSlots
        println(s"\t\tOUT (without prefered slots) PAR = ${Metric.par(resultWithoutPreferedSlots)}, flex pos -> ${userFlexibleLoadsPositionToString(resultWithoutPreferedSlots)}")

        val resultWithPreferedSlots = Rescheduler.reschedule(user, userPreferedSlots, verbose = false)
        resultsWithPreferedSlots = resultWithPreferedSlots :: resultsWithPreferedSlots
        println(s"\t\tOUT (with prefered slots) PAR = ${Metric.par(resultWithPreferedSlots)}, flex pos -> ${userFlexibleLoadsPositionToString(resultWithPreferedSlots)}")
      }

      println(s"OUT total (without prefered slots) PAR = ${Metric.par(SpanSlotAccumulatedLoad(-2, 0, resultsWithoutPreferedSlots))}")
      println(s"OUT total (with prefered slots) PAR = ${Metric.par(SpanSlotAccumulatedLoad(-3, 0, resultsWithPreferedSlots))}")
    }
  }

  def getUsers: List[SpanSlotAccumulatedLoad] = {
    val usersSimulation: List[SpanSlotAccumulatedLoad] = List(
      SpanSlotAccumulatedLoad(100, 0, List(
        SpanSlotFixedLoad(101, 0, Vector(1, 3, 1)),
        SpanSlotFlexibleLoad(151, 0, Vector(2))
      )),

      SpanSlotAccumulatedLoad(200, 0, List(
        SpanSlotFixedLoad(201, 0, Vector(0, 0, 0)),
        SpanSlotFlexibleLoad(251, 0, Vector(1))
      )),

      SpanSlotAccumulatedLoad(300, 0, List(
        SpanSlotFixedLoad(301, 0, Vector(0, 0, 0)),
        SpanSlotFlexibleLoad(351, 0, Vector(2))
      ))
    )

    usersSimulation
  }

  def userFlexibleLoadsPositionToString(user: SpanSlotAccumulatedLoad): String = {
    val flexLoadsPosition = user.flexibleLoads.toList.sortBy(_.id).map(_.positionInT)

    flexLoadsPosition.toString()
  }
}

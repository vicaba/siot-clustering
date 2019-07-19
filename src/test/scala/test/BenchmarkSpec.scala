package test

import org.scalatest._
import Load._
import metrics.Metric
import test.RescheduleType.RescheduleType
import test.reschedulermetrics.{BiasedAverageDistanceTransformation, MetricTransformation}

import scala.util.Try

class BenchmarkSpec extends FeatureSpec with GivenWhenThen with Matchers {

  val metricTransformation = new BiasedAverageDistanceTransformation()

  feature(
    "Benchmark between the input PAR and the output PAR of the two alternatives (with and without prefered slots per user)") {
    scenario("3 slots with 3 users and a flexible load each") {
      val users: List[SpanSlotAccumulatedLoad] = List(
        SpanSlotAccumulatedLoad(100,
                                0,
                                List(
                                  SpanSlotFixedLoad(101, 0, Vector(2, 4, 1)),
                                  SpanSlotFlexibleLoad(151, 0, Vector(3))
                                )),
        SpanSlotAccumulatedLoad(200,
                                0,
                                List(
                                  SpanSlotFixedLoad(201, 0, Vector(2, 4, 1)),
                                  SpanSlotFlexibleLoad(251, 0, Vector(3))
                                )),
        SpanSlotAccumulatedLoad(300,
                                0,
                                List(
                                  SpanSlotFixedLoad(301, 0, Vector(2, 4, 1)),
                                  SpanSlotFlexibleLoad(351, 0, Vector(3))
                                ))
      )

      val expectedTotalLoad: List[Vector[Double]] = List(
        Vector[Double](12, 12, 6),
        Vector[Double](6, 12, 12),
        Vector[Double](9, 12, 9)
      )

      executeTest(
        users,
        expectedTotalLoad,
        metricTransformation,
        testVerbose = true,
        schedulerVerbose = true,
        printLoads = true
      )
    }

    scenario("All the same") {
      val users: List[SpanSlotAccumulatedLoad] = List(
        SpanSlotAccumulatedLoad(100,
                                0,
                                List(
                                  SpanSlotFixedLoad(101, 0, Vector(1, 1, 1)),
                                  SpanSlotFlexibleLoad(151, 0, Vector(1, 1, 1))
                                )),
        SpanSlotAccumulatedLoad(200,
                                0,
                                List(
                                  SpanSlotFixedLoad(201, 0, Vector(1, 1, 1)),
                                  SpanSlotFlexibleLoad(251, 0, Vector(1, 1, 1))
                                ))
      )

      val expectedTotalLoad: List[Vector[Double]] = List(
        Vector[Double](4, 4, 4)
      )

      executeTest(
        users,
        expectedTotalLoad,
        metricTransformation,
        testVerbose = true,
        schedulerVerbose = true,
        printLoads = true
      )
    }

    scenario("With a gap, 2 users with 1 flexible load each to the middle") {
      val users: List[SpanSlotAccumulatedLoad] = List(
        SpanSlotAccumulatedLoad(100,
                                0,
                                List(
                                  SpanSlotFixedLoad(101, 0, Vector(4, 0, 4)),
                                  SpanSlotFlexibleLoad(151, 0, Vector(3))
                                )),
        SpanSlotAccumulatedLoad(200,
                                0,
                                List(
                                  SpanSlotFixedLoad(201, 0, Vector(4, 0, 4)),
                                  SpanSlotFlexibleLoad(251, 0, Vector(3))
                                ))
      )

      val expectedTotalLoad: List[Vector[Double]] = List(
        Vector[Double](8, 6, 8)
      )

      executeTest(
        users,
        expectedTotalLoad,
        metricTransformation,
        testVerbose = true,
        schedulerVerbose = true,
        printLoads = true
      )
    }

    scenario("With a gap, 2 users with 1 flexible load, the highest to the middle the other to one of the sides") {
      val users: List[SpanSlotAccumulatedLoad] = List(
        SpanSlotAccumulatedLoad(500,
                                0,
                                List(
                                  SpanSlotFixedLoad(101, 0, Vector(4, 0, 4)),
                                  SpanSlotFlexibleLoad(151, 0, Vector(11))
                                )),
        SpanSlotAccumulatedLoad(600,
                                0,
                                List(
                                  SpanSlotFixedLoad(201, 0, Vector(4, 0, 4)),
                                  SpanSlotFlexibleLoad(251, 0, Vector(12))
                                ))
      )

      val expectedTotalLoad: List[Vector[Double]] = List(
        Vector[Double](19, 12, 8),
        Vector[Double](8, 12, 19)
      )

      executeTest(
        users,
        expectedTotalLoad,
        metricTransformation,
        testVerbose = true,
        schedulerVerbose = true,
        printLoads = true
      )
    }

    scenario("6 slots with some low loads in between, 3 users") {
      val users: List[SpanSlotAccumulatedLoad] = List(
        SpanSlotAccumulatedLoad(100,
                                0,
                                List(
                                  SpanSlotFixedLoad(101, 0, Vector(3, 3, 3, 0, 1, 2)),
                                  SpanSlotFlexibleLoad(151, 0, Vector(2, 3))
                                )),
        SpanSlotAccumulatedLoad(200,
                                0,
                                List(
                                  SpanSlotFixedLoad(201, 0, Vector(3, 1, 0, 5, 2, 1)),
                                  SpanSlotFlexibleLoad(251, 0, Vector(5, 3))
                                )),
        SpanSlotAccumulatedLoad(300,
                                0,
                                List(
                                  SpanSlotFixedLoad(301, 0, Vector(2, 0, 4, 5, 3, 1)),
                                  SpanSlotFlexibleLoad(351, 0, Vector(4, 4))
                                ))
      )

      val expectedTotalLoad: List[Vector[Double]] = List(
        Vector[Double](10, 12, 10, 10, 8),
        Vector[Double](8, 9, 10, 12, 11)
      )

      executeTest(
        users,
        expectedTotalLoad,
        metricTransformation,
        testVerbose = true,
        schedulerVerbose = true,
        printLoads = true
      )
    }
  }

  def loadInSlots(load: Load, fromSlot: Int, toSlot: Int): List[String] = {
    val inSlot  = "-"
    val outSlot = ""

    var strings: List[String] = List()
    for (i <- fromSlot to toSlot) {
      if (i >= load.positionInT && i < (load.positionInT + load.span)) strings = inSlot :: strings
      else strings = outSlot :: strings
    }

    strings.reverse
  }

  def executeTest(
      users: List[SpanSlotAccumulatedLoad],
      expectedTotalLoad: List[Vector[Double]],
      metricTransformation: MetricTransformation,
      printLoads: Boolean = false,
      testVerbose: Boolean = false,
      schedulerVerbose: Boolean = false
  ): Unit = {

    val benchmarkResult =
      executeBenchmark(users, metricTransformation, schedulerVerbose)

    printTable(users, benchmarkResult, printLoads, testVerbose)

    val accumulatedLoadWithoutPriority = SpanSlotAccumulatedLoad(-1, 0, benchmarkResult.resultsWithoutPriority)
    val accumulatedLoadWithPriority    = SpanSlotAccumulatedLoad(-1, 0, benchmarkResult.resultsWithPriority)

    expectedTotalLoad match {
      case Nil           =>
      case x :: Nil      => accumulatedLoadWithPriority.amplitudePerSlot shouldBe x
      case x :: y :: Nil => List(accumulatedLoadWithPriority.amplitudePerSlot) should contain oneOf (x, y)
      case x :: y :: xs  => List(accumulatedLoadWithPriority.amplitudePerSlot) should contain oneOf (x, y, xs: _*)
    }
  }

  def printTable(users: List[SpanSlotAccumulatedLoad],
                 benchmarkResult: BenchmarkResult,
                 printLoads: Boolean,
                 testVerbose: Boolean): Unit = {

    val resultsWithoutPriority = benchmarkResult.resultsWithoutPriority
    val resultsWithPriority    = benchmarkResult.resultsWithPriority
    val usersPreferredSlots    = benchmarkResult.usersPreferredSlots

    val initialPar = computePar(users)
    if (testVerbose) info(s"IN total PAR = $initialPar")

    for (i <- users.indices) {
      val user                   = users(i)
      val userPreferredSlots     = usersPreferredSlots(i)
      val resultWithoutPriority  = resultsWithoutPriority(i)
      val userParWithoutPriority = computePar(resultWithoutPriority)
      val resultWithPriority     = resultsWithPriority(i)
      val userParWithPriority    = computePar(resultWithPriority)

      val fromSlot = user.positionInT
      val toSlot   = user.positionInT + user.span - 1

      val userInitialTable = new TableList("load_id" :: (fromSlot to toSlot).map(_.toString).toList)
      for (load <- user.loads.toList.sortBy(_.id)) {
        userInitialTable.addRow(load.id.toString :: loadInSlots(load, fromSlot, toSlot))
      }
      userInitialTable.addRow("total_load" :: user.amplitudePerSlot.toList.map(_.toString))
      val userTableWithoutPriority = new TableList("load_id" :: (fromSlot to toSlot).map(_.toString).toList)
      for (load <- resultWithoutPriority.loads.toList.sortBy(_.id)) {
        userTableWithoutPriority.addRow(load.id.toString :: loadInSlots(load, fromSlot, toSlot))
      }
      userTableWithoutPriority.addRow("total_load" :: resultWithoutPriority.amplitudePerSlot.toList.map(_.toString))
      val userTableWithPriority = new TableList("load_id" :: (fromSlot to toSlot).map(_.toString).toList)
      for (load <- resultWithPriority.loads.toList.sortBy(_.id)) {
        userTableWithPriority.addRow(load.id.toString :: loadInSlots(load, fromSlot, toSlot))
      }
      userTableWithPriority.addRow("total_load" :: resultWithPriority.amplitudePerSlot.toList.map(_.toString))
      if (printLoads) {
        println(s"User ${user.id} | Prefered slots -> ${preferredSlotsToString(userPreferredSlots)}")
        println(s"\tInitial | PAR = ${computePar(user)}")
        userInitialTable.print(1)
        println(s"\tOutput without priority | PAR = $userParWithoutPriority")
        userTableWithoutPriority.print(1)
        println(s"\tOutput with priority | PAR = $userParWithPriority")
        userTableWithPriority.print(1)
        println()
      }

    }

    val accumulatedLoadWithoutPriority = SpanSlotAccumulatedLoad(-1, 0, resultsWithoutPriority)
    val accumulatedLoadWithPriority    = SpanSlotAccumulatedLoad(-1, 0, resultsWithPriority)

    if (testVerbose) info(accumulatedLoadWithPriority.amplitudePerSlot.toString())

    val outputParWithoutPriority = computePar(resultsWithoutPriority)
    if (testVerbose) info(s"OUT total (without priority) PAR = $outputParWithoutPriority")

    val outputParWithPriority = computePar(resultsWithPriority)
    if (testVerbose) info(s"OUT total (with priority) PAR = $outputParWithPriority")

    val initialAccumulated = SpanSlotAccumulatedLoad(-1, 0, users)
    val initialTotalTable = new TableList(
      "load_id" :: (initialAccumulated.positionInT until (initialAccumulated.positionInT + initialAccumulated.span))
        .map(_.toString)
        .toList)
    for (load <- users.flatMap(_.loads).sortBy(_.id)) {
      initialTotalTable.addRow(
        load.id.toString :: loadInSlots(load,
                                        initialAccumulated.positionInT,
                                        initialAccumulated.positionInT + initialAccumulated.span - 1))
    }
    initialTotalTable.addRow("total_load" :: initialAccumulated.amplitudePerSlot.toList.map(_.toString))
    val totalTableWithoutPriority = new TableList(
      "load_id" :: (accumulatedLoadWithoutPriority.positionInT until (accumulatedLoadWithoutPriority.positionInT + accumulatedLoadWithoutPriority.span))
        .map(_.toString)
        .toList)
    for (load <- resultsWithoutPriority.flatMap(_.loads).sortBy(_.id)) {
      totalTableWithoutPriority.addRow(
        load.id.toString :: loadInSlots(
          load,
          accumulatedLoadWithoutPriority.positionInT,
          accumulatedLoadWithoutPriority.positionInT + accumulatedLoadWithoutPriority.span - 1))
    }
    totalTableWithoutPriority.addRow(
      "total_load" :: accumulatedLoadWithoutPriority.amplitudePerSlot.toList.map(_.toString))
    val totalTableWithPriority = new TableList(
      "load_id" :: (accumulatedLoadWithPriority.positionInT until (accumulatedLoadWithPriority.positionInT + accumulatedLoadWithPriority.span))
        .map(_.toString)
        .toList)
    for (load <- resultsWithPriority.flatMap(_.loads).sortBy(_.id)) {
      totalTableWithPriority.addRow(
        load.id.toString :: loadInSlots(load,
                                        accumulatedLoadWithPriority.positionInT,
                                        accumulatedLoadWithPriority.positionInT + accumulatedLoadWithPriority.span - 1))
    }
    totalTableWithPriority.addRow("total_load" :: accumulatedLoadWithPriority.amplitudePerSlot.toList.map(_.toString))

    if (printLoads) {
      println(
        s"Average load per user = ${users.map(_.totalEnergy).sum / SpanSlotAccumulatedLoad(-1, 0, users).span / users.size}")
      println(s"Initial total loads | PAR = $initialPar")
      initialTotalTable.print(1)
      println(s"Output total loads without priority | PAR = $outputParWithoutPriority")
      totalTableWithoutPriority.print(1)
      println(s"Output total loads with priority | PAR = $outputParWithPriority")
      totalTableWithPriority.print(1)
      println()
    }

  }

  def preferredSlotsToString(preferredSlots: List[Int]): String = {
    val sb = StringBuilder.newBuilder

    for (i <- preferredSlots.indices) {
      if (i != 0) sb.append(", ")
      sb.append(preferredSlots(i).toString)
    }

    sb.toString
  }

  case class BenchmarkResult(resultsWithoutPriority: List[SpanSlotAccumulatedLoad],
                             resultsWithPriority: List[SpanSlotAccumulatedLoad],
                             usersPreferredSlots: List[List[Int]])

  def executeBenchmark(
      users: List[SpanSlotAccumulatedLoad],
      metricTransformation: MetricTransformation,
      verbose: Boolean = false
  ): BenchmarkResult = {

    val numberOfSlots    = SpanSlotAccumulatedLoad(-1, 0, users).span
    val allFlexibleLoads = users.flatMap(_.flexibleLoads)
    val windowSize       = Try(allFlexibleLoads.map(_.span).sum / allFlexibleLoads.size).getOrElse(1)
    println(s"Num of slots = $numberOfSlots, SlotsWindowSize = $windowSize")
    val schedulerPreferredSlots =
      UserAllocator.allocate(users = users, numberOfSlots = numberOfSlots, windowSize = windowSize)

    val referenceAverage = users.map(_.totalEnergy).sum / numberOfSlots / users.size
    //info(s"Reference average = $referenceAverage")

    val res = users.zip(schedulerPreferredSlots).map {
      case (user, schedulingPreferredSlotsForUser) =>
        val resultWithoutPreferredSlots =
          Rescheduler.reschedule(user, metricTransformation = metricTransformation, referenceAverage = referenceAverage)

        val resultWithPreferredSlots = Rescheduler.reschedule(user,
                                                              schedulingPreferredSlotsForUser,
                                                              metricTransformation = metricTransformation,
                                                              referenceAverage = referenceAverage,
                                                              verbose = verbose)

        (resultWithoutPreferredSlots, resultWithPreferredSlots)
    }

    BenchmarkResult(res.map(_._1), res.map(_._2), schedulerPreferredSlots)

  }

  def computePar(loads: Iterable[Load]): Double = Metric.par(SpanSlotAccumulatedLoad(-1, 0, loads))
  def computePar(load: Load): Double            = Metric.par(SpanSlotAccumulatedLoad(-1, 0, load))
}

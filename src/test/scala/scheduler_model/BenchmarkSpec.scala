package scheduler_model

import breeze.linalg.DenseVector
import org.scalatest._
import metrics.Metric
import scheduler_model.load._
import scheduler_model.scheduler.SchedulerAlgorithm
import scheduler_model.scheduler.metric_transformer.{BiasedAverageDistanceTransformation, MetricTransformation}
import scheduler_model.user_allocator.UserAllocator
import test.TableList
import types.clusterer.DataTypeMetadata

import scala.util.Try

class BenchmarkSpec extends FeatureSpec with GivenWhenThen with Matchers {

  val metricTransformation = new BiasedAverageDistanceTransformation()

  def enableLog(bool: Boolean)(f: => Any): Unit = if (bool) f

  object DefaultConfiguration {
    val enableTestVerbose      = true
    val enableSchedulerVerbose = false
    val enablePrintLoads       = true
    val enableGenerateTables   = false
  }

  feature(
    "Benchmark between the input PAR and the output PAR of the two alternatives (with and without prefered slots per user)") {
    scenario("3 slots with 3 users and a flexible load each") {
      val scenarioName = "3 slots with 3 users and a flexible load each"
      val users: List[AccumulatedLoad] = List(
        AccumulatedLoad(100,
                        100,
                        "100",
                        List(
                          FixedLoad(101, 101, "101", DenseVector(2, 4, 1)),
                          FlexibleLoad(151, 151, "151", 0, DenseVector(3))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
        AccumulatedLoad(200,
                        200,
                        "200",
                        List(
                          FixedLoad(201, 201, "201", DenseVector(2, 4, 1)),
                          FlexibleLoad(251, 251, "251", 0, DenseVector(3))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
        AccumulatedLoad(300,
                        300,
                        "300",
                        List(
                          FixedLoad(301, 301, "301", DenseVector(2, 4, 1)),
                          FlexibleLoad(351, 351, "351", 0, DenseVector(3))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3))
      )

      val expectedTotalLoad: List[DenseVector[Double]] = List(
        DenseVector[Double](12, 12, 6),
        DenseVector[Double](6, 12, 12),
        DenseVector[Double](9, 12, 9)
      )

      executeScenario(
        users,
        expectedTotalLoad,
        metricTransformation,
        enableTestVerbose = DefaultConfiguration.enableTestVerbose,
        enableSchedulerVerbose = DefaultConfiguration.enableSchedulerVerbose,
        enablePrintLoads = DefaultConfiguration.enablePrintLoads,
        enableGenerateTables = DefaultConfiguration.enableGenerateTables,
        scenarioName = scenarioName
      )
    }

    scenario("All users have the same loads") {
      val scenarioName = "All users have the same loads"

      val users: List[AccumulatedLoad] = List(
        AccumulatedLoad(100,
                        100,
                        "100",
                        List(
                          FixedLoad(101, 101, "101", DenseVector(1, 1, 1)),
                          FlexibleLoad(151, 151, "151", 0, DenseVector(1, 1, 1))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
        AccumulatedLoad(200,
                        200,
                        "200",
                        List(
                          FixedLoad(201, 201, "201", DenseVector(1, 1, 1)),
                          FlexibleLoad(251, 251, "251", 0, DenseVector(1, 1, 1))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3))
      )

      val expectedTotalLoad: List[DenseVector[Double]] = List(
        DenseVector[Double](4, 4, 4)
      )

      executeScenario(
        users,
        expectedTotalLoad,
        metricTransformation,
        enableTestVerbose = DefaultConfiguration.enableTestVerbose,
        enableSchedulerVerbose = DefaultConfiguration.enableSchedulerVerbose,
        enablePrintLoads = DefaultConfiguration.enablePrintLoads,
        enableGenerateTables = DefaultConfiguration.enableGenerateTables,
        scenarioName = scenarioName
      )
    }

    scenario("With a gap, 2 users with 1 flexible load each to the middle") {
      val scenarioName = "With a gap, 2 users with 1 flexible load each to the middle"
      val users: List[AccumulatedLoad] = List(
        AccumulatedLoad(100,
                        100,
                        "100",
                        List(
                          FixedLoad(101, 101, "101", DenseVector(4, 0, 4)),
                          FlexibleLoad(151, 151, "151", 0, DenseVector(3))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
        AccumulatedLoad(200,
                        200,
                        "200",
                        List(
                          FixedLoad(201, 201, "201", DenseVector(4, 0, 4)),
                          FlexibleLoad(251, 251, "251", 0, DenseVector(3))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3))
      )

      val expectedTotalLoad: List[DenseVector[Double]] = List(
        DenseVector[Double](8, 6, 8)
      )

      executeScenario(
        users,
        expectedTotalLoad,
        metricTransformation,
        enableTestVerbose = DefaultConfiguration.enableTestVerbose,
        enableSchedulerVerbose = DefaultConfiguration.enableSchedulerVerbose,
        enablePrintLoads = DefaultConfiguration.enablePrintLoads,
        enableGenerateTables = DefaultConfiguration.enableGenerateTables,
        scenarioName = scenarioName
      )
    }

    scenario("With a gap, 2 users with 1 flexible load, the highest to the middle the other to one of the sides") {
      val scenarioName =
        "With a gap, 2 users with 1 flexible load, the highest to the middle the other to one of the sides"

      val users: List[AccumulatedLoad] = List(
        AccumulatedLoad(500,
                        500,
                        "500",
                        List(
                          FixedLoad(101, 101, "101", DenseVector(4, 0, 4)),
                          FlexibleLoad(151, 151, "151", 0, DenseVector(11))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3)),
        AccumulatedLoad(600,
                        600,
                        "600",
                        List(
                          FixedLoad(201, 201, "201", DenseVector(4, 0, 4)),
                          FlexibleLoad(251, 251, "251", 0, DenseVector(12))
                        ))(DataTypeMetadata.generateDataTypeMetadata(forColumns = 3))
      )

      val expectedTotalLoad: List[DenseVector[Double]] = List(
        DenseVector[Double](19, 12, 8),
        DenseVector[Double](8, 12, 19)
      )

      executeScenario(
        users,
        expectedTotalLoad,
        metricTransformation,
        enableTestVerbose = DefaultConfiguration.enableTestVerbose,
        enableSchedulerVerbose = DefaultConfiguration.enableSchedulerVerbose,
        enablePrintLoads = DefaultConfiguration.enablePrintLoads,
        enableGenerateTables = DefaultConfiguration.enableGenerateTables,
        scenarioName = scenarioName
      )
    }

    scenario("6 slots with some low loads in between, 3 users") {
      val scenarioName = "6 slots with some low loads in between, 3 users"

      val users: List[AccumulatedLoad] = List(
        AccumulatedLoad.AutoSpanFromLoads(100,
                        100,
                        "100",
                        List(
                          FixedLoad(101, 101, "101", DenseVector(3, 3, 3, 0, 1, 2)),
                          FlexibleLoad(151, 151, "151", 0, DenseVector(2, 3))
                        )),
        AccumulatedLoad.AutoSpanFromLoads(200,
                        200,
                        "200",
                        List(
                          FixedLoad(201, 201, "201", DenseVector(3, 1, 0, 5, 2, 1)),
                          FlexibleLoad(251, 251, "251", 0, DenseVector(5, 3))
                        )),
        AccumulatedLoad.AutoSpanFromLoads(300,
                        300,
                        "300",
                        List(
                          FixedLoad(301, 301, "301", DenseVector(2, 0, 4, 5, 3, 1)),
                          FlexibleLoad(351, 351, "351", 0, DenseVector(4, 4))
                        ))
      )

      val expectedTotalLoad: List[DenseVector[Double]] = List(
        DenseVector[Double](10, 12, 10, 10, 8),
        DenseVector[Double](8, 9, 10, 12, 11)
      )

      executeScenario(
        users,
        expectedTotalLoad,
        metricTransformation,
        enableTestVerbose = DefaultConfiguration.enableTestVerbose,
        enableSchedulerVerbose = DefaultConfiguration.enableSchedulerVerbose,
        enablePrintLoads = DefaultConfiguration.enablePrintLoads,
        enableGenerateTables = DefaultConfiguration.enableGenerateTables,
        scenarioName = scenarioName
      )
    }
  }

  def loadInSlots(load: Load, fromSlot: Int, toSlot: Int): List[String] = {
    val inSlot  = "-"
    val outSlot = ""

    var strings: List[String] = List()
    for (i <- fromSlot to toSlot) {
      if (i >= load.startPositionInTime && i < (load.startPositionInTime + load.span)) strings = inSlot :: strings
      else strings = outSlot :: strings
    }

    strings.reverse
  }

  def executeScenario(
      users: List[AccumulatedLoad],
      expectedTotalLoad: List[DenseVector[Double]],
      metricTransformation: MetricTransformation,
      enableTestVerbose: Boolean,
      enableSchedulerVerbose: Boolean,
      enablePrintLoads: Boolean,
      enableGenerateTables: Boolean,
      scenarioName: String = ""
  ): Unit = {

    val benchmarkResult =
      executeBenchmark(users, metricTransformation, enableSchedulerVerbose)

    generateLoadsLog(users, benchmarkResult, enablePrintLoads, enableTestVerbose, scenarioName, enableGenerateTables)

    val accumulatedLoadWithoutPriority =
      AccumulatedLoad.AutoSpanFromLoads(-1, -1, "accumulatedLoadWithoutPriority", benchmarkResult.resultsWithoutPriority)
    val accumulatedLoadWithPriority =
      AccumulatedLoad.AutoSpanFromLoads(-1, -1, "accumulatedLoadWithPriority", benchmarkResult.resultsWithPriority)

    Metric.par(accumulatedLoadWithPriority) should be <= Metric.par(accumulatedLoadWithoutPriority)

    expectedTotalLoad match {
      case Nil           =>
      case x :: Nil      => accumulatedLoadWithPriority.amplitudePerSlot shouldBe x
      case x :: y :: Nil => List(accumulatedLoadWithPriority.amplitudePerSlot) should contain oneOf (x, y)
      case x :: y :: xs  => List(accumulatedLoadWithPriority.amplitudePerSlot) should contain oneOf (x, y, xs: _*)
    }

    benchmarkResult.resultsWithPriority.map(_.totalEnergy).sum shouldBe users.map(_.totalEnergy).sum
  }

  def generateLoadsLog(users: List[AccumulatedLoad],
                       benchmarkResult: BenchmarkResult,
                       enablePrintLoads: Boolean,
                       enableTestVerbose: Boolean,
                       scenarioName: String,
                       enableGenerateTables: Boolean): Unit = {

    val resultsWithoutPriority = benchmarkResult.resultsWithoutPriority
    val resultsWithPriority    = benchmarkResult.resultsWithPriority
    val usersPreferredSlots    = benchmarkResult.usersPreferredSlots

    val initialPar = Metric.par(users)

    val outScenarioName = "Scenario name: " + scenarioName
    println("START OF SCENARIO")
    println("Scenario name: " + scenarioName)
    println(List.fill(outScenarioName.length)('-').mkString(""))

    enableLog(enableTestVerbose) {
      println(s"IN total PAR = $initialPar")
      println(
        s"Average load per user = ${users.map(_.totalEnergy).sum / users.head.span / users.size}")
    }

    for (i <- users.indices) {
      val user                   = users(i)
      val userPreferredSlots     = usersPreferredSlots(i)
      val resultWithoutPriority  = resultsWithoutPriority(i)
      val userParWithoutPriority = Metric.par(resultWithoutPriority)
      val resultWithPriority     = resultsWithPriority(i)
      val userParWithPriority    = Metric.par(resultWithPriority)

      val fromSlot = user.startPositionInTime
      val toSlot   = user.startPositionInTime + user.span - 1

      enableLog(enablePrintLoads) {
        println(s"User ${user.id} | Prefered slots -> ${preferredSlotsToString(userPreferredSlots)}")
        println(s"\tInitial | PAR = ${Metric.par(user)}")
      }
      enableLog(enableGenerateTables) {
        val userInitialTable = new TableList("load_id" :: (fromSlot to toSlot).map(_.toString).toList)
        for (load <- user.loads.toList.sortBy(_.id)) {
          userInitialTable.addRow(load.id.toString :: loadInSlots(load, fromSlot, toSlot))
        }
        userInitialTable.addRow("total_load" :: user.amplitudePerSlot.toDenseVector.toScalaVector().toList.map(_.toString))
        userInitialTable.print(1)
      }
      enableLog(enablePrintLoads)(println(s"\tOutput without priority | PAR = $userParWithoutPriority"))
      enableLog(enableGenerateTables) {
        val userTableWithoutPriority = new TableList("load_id" :: (fromSlot to toSlot).map(_.toString).toList)
        for (load <- resultWithoutPriority.loads.toList.sortBy(_.id)) {
          userTableWithoutPriority.addRow(load.id.toString :: loadInSlots(load, fromSlot, toSlot))
        }
        userTableWithoutPriority.addRow("total_load" :: resultWithoutPriority.amplitudePerSlot.toDenseVector.toScalaVector().toList.map(_.toString))
        userTableWithoutPriority.print(1)
      }
      enableLog(enablePrintLoads)(println(s"\tOutput with priority | PAR = $userParWithPriority"))
      enableLog(enableGenerateTables) {
        val userTableWithPriority = new TableList("load_id" :: (fromSlot to toSlot).map(_.toString).toList)
        for (load <- resultWithPriority.loads.toList.sortBy(_.id)) {
          userTableWithPriority.addRow(load.id.toString :: loadInSlots(load, fromSlot, toSlot))
        }
        userTableWithPriority.addRow("total_load" :: resultWithPriority.amplitudePerSlot.toDenseVector.toScalaVector().toList.map(_.toString))
        userTableWithPriority.print(1)
      }
      enableLog(enableGenerateTables | enablePrintLoads)(println())
    }

    val accumulatedLoadWithoutPriority =
      AccumulatedLoad.AutoSpanFromLoads(-1, -1, "accumulatedLoadWithoutPriority", resultsWithoutPriority)
    val accumulatedLoadWithPriority    =
      AccumulatedLoad.AutoSpanFromLoads(-1, -1, "accumulatedLoadWithPriority", resultsWithPriority)

    val outputParWithoutPriority = Metric.par(resultsWithoutPriority)
    enableLog(enableTestVerbose) {
      println(s"OUT total (without priority) PAR = $outputParWithoutPriority")
      println(accumulatedLoadWithoutPriority.amplitudePerSlot.toString())
    }

    val outputParWithPriority = Metric.par(resultsWithPriority)
    enableLog(enableTestVerbose) {
      println(s"OUT total (with priority) PAR = $outputParWithPriority")
      println(accumulatedLoadWithPriority.amplitudePerSlot.toString())
    }

    val initialAccumulated = AccumulatedLoad.AutoSpanFromLoads(-1, -1, "-1", users)
    enableLog(enableGenerateTables) {

      val initialTotalTable = new TableList(
        "load_id" :: (initialAccumulated.startPositionInTime until (initialAccumulated.startPositionInTime + initialAccumulated.span))
          .map(_.toString)
          .toList)
      for (load <- users.flatMap(_.loads).sortBy(_.id)) {
        initialTotalTable.addRow(
          load.id.toString :: loadInSlots(load,
                                          initialAccumulated.startPositionInTime,
                                          initialAccumulated.startPositionInTime + initialAccumulated.span - 1))
      }
      initialTotalTable.addRow("total_load" :: initialAccumulated.amplitudePerSlot.toDenseVector.toScalaVector().toList.map(_.toString))

      initialTotalTable.print(1)
    }

    enableLog(enableGenerateTables) {
      val totalTableWithoutPriority = new TableList(
        "load_id" :: (accumulatedLoadWithoutPriority.startPositionInTime until (accumulatedLoadWithoutPriority.startPositionInTime + accumulatedLoadWithoutPriority.span))
          .map(_.toString)
          .toList)
      for (load <- resultsWithoutPriority.flatMap(_.loads).sortBy(_.id)) {
        totalTableWithoutPriority.addRow(
          load.id.toString :: loadInSlots(
            load,
            accumulatedLoadWithoutPriority.startPositionInTime,
            accumulatedLoadWithoutPriority.startPositionInTime + accumulatedLoadWithoutPriority.span - 1))
      }
      totalTableWithoutPriority.addRow(
        "total_load" :: accumulatedLoadWithoutPriority.amplitudePerSlot.toDenseVector.toScalaVector().toList.map(_.toString))

      println(s"Output total loads without priority | PAR = $outputParWithoutPriority")
      totalTableWithoutPriority.print(1)
    }

    enableLog(enableGenerateTables) {
      val totalTableWithPriority = new TableList(
        "load_id" :: (accumulatedLoadWithPriority.startPositionInTime until (accumulatedLoadWithPriority.startPositionInTime + accumulatedLoadWithPriority.span))
          .map(_.toString)
          .toList)
      for (load <- resultsWithPriority.flatMap(_.loads).sortBy(_.id)) {
        totalTableWithPriority.addRow(
          load.id.toString :: loadInSlots(
            load,
            accumulatedLoadWithPriority.startPositionInTime,
            accumulatedLoadWithPriority.startPositionInTime + accumulatedLoadWithPriority.span - 1))
      }
      totalTableWithPriority.addRow("total_load" :: accumulatedLoadWithPriority.amplitudePerSlot.toDenseVector.toScalaVector().toList.map(_.toString))

      println(s"Output total loads with priority | PAR = $outputParWithPriority")
      totalTableWithPriority.print(1)
    }

    enableLog(enablePrintLoads | enableGenerateTables)(println())

    println("END OF SCENARIO")
    println()

  }

  def preferredSlotsToString(preferredSlots: List[Int]): String = {
    val sb = StringBuilder.newBuilder

    for (i <- preferredSlots.indices) {
      if (i != 0) sb.append(", ")
      sb.append(preferredSlots(i).toString)
    }

    sb.toString
  }

  case class BenchmarkResult(resultsWithoutPriority: List[AccumulatedLoad],
                             resultsWithPriority: List[AccumulatedLoad],
                             usersPreferredSlots: List[List[Int]])

  def executeBenchmark(
      users: List[AccumulatedLoad],
      metricTransformation: MetricTransformation,
      verbose: Boolean = false
  ): BenchmarkResult = {

    val numberOfSlots    = users.head.span
    enableLog(verbose)(println(s"Num of slots = $numberOfSlots"))
    val schedulerPreferredSlots =
      UserAllocator.allocate(users)

    val referenceAverage = users.map(_.totalEnergy).sum / numberOfSlots / users.size
    //info(s"Reference average = $referenceAverage")

    val res = users.zip(schedulerPreferredSlots).map {
      case (user, schedulingPreferredSlotsForUser) =>
        val resultWithoutPreferredSlots =
          SchedulerAlgorithm
            .reschedule(LoadOps.copy(user), metricTransformation = metricTransformation, referenceAverage = referenceAverage)

        val resultWithPreferredSlots = SchedulerAlgorithm.reschedule(LoadOps.copy(user),
                                                                     schedulingPreferredSlotsForUser,
                                                                     metricTransformation = metricTransformation,
                                                                     referenceAverage = referenceAverage,
                                                                     verbose = verbose)

        (resultWithoutPreferredSlots, resultWithPreferredSlots)
    }

    BenchmarkResult(res.map(_._1), res.map(_._2), schedulerPreferredSlots)

  }

}

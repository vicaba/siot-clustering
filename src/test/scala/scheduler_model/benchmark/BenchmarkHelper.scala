package scheduler_model.benchmark

import breeze.linalg.DenseVector
import metrics.Metric
import org.scalatest.Matchers._
import scheduler_model.load.{AccumulatedLoad, Load, LoadOps}
import scheduler_model.scheduler.SchedulerAlgorithm
import scheduler_model.scheduler.metric_transformer.MetricTransformation
import scheduler_model.user_allocator.UserAllocator
import test.TableList

object BenchmarkHelper {

  def enableLog(bool: Boolean)(f: => Any): Unit = if (bool) f

  object DefaultConfiguration {
    val enableTestVerbose = true
    val enableSchedulerVerbose = false
    val enablePrintLoads = true
    val enableGenerateTables = false
  }

  def loadInSlots(load: Load, fromSlot: Int, toSlot: Int): List[String] = {
    val inSlot = "-"
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
      case Nil =>
      case x :: Nil => accumulatedLoadWithPriority.amplitudePerSlot shouldBe x
      case x :: y :: Nil => List(accumulatedLoadWithPriority.amplitudePerSlot) should contain oneOf(x, y)
      case x :: y :: xs => List(accumulatedLoadWithPriority.amplitudePerSlot) should contain oneOf(x, y, xs: _*)
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
    val resultsWithPriority = benchmarkResult.resultsWithPriority
    val usersPreferredSlots = benchmarkResult.usersPreferredSlots

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
      val user = users(i)
      val userPreferredSlots = usersPreferredSlots(i)
      val resultWithoutPriority = resultsWithoutPriority(i)
      val userParWithoutPriority = Metric.par(resultWithoutPriority)
      val resultWithPriority = resultsWithPriority(i)
      val userParWithPriority = Metric.par(resultWithPriority)

      val fromSlot = user.startPositionInTime
      val toSlot = user.startPositionInTime + user.span - 1

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
    val accumulatedLoadWithPriority =
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

    val numberOfSlots = users.head.span
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

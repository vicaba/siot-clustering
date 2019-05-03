package test

import breeze.linalg._
import breeze.stats._
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.chart.LineChart
import javafx.scene.chart.NumberAxis
import javafx.scene.chart.XYChart
import javafx.stage.Stage
import types.ops.MirrorImage.MirroredSyntheticDataType

object LineChartSample {

  def main(args: Array[String]): Unit = start()

  def start(data: List[(Number, Number)] = seriesData): Unit = {
    seriesData = data
    Application.launch(classOf[LineChartSample])
  }

  private var seriesData = List[(Number, Number)](
    (1, 23),
    (2, 27)
  )

}

class LineChartSample extends Application {
  override def start(stage: Stage): Unit = {
    stage.setTitle("Line Chart Sample")
    //defining the axes
    val xAxis = new NumberAxis
    val yAxis = new NumberAxis
    xAxis.setLabel("Number of Month")
    //creating the chart
    val lineChart = new LineChart[Number, Number](xAxis, yAxis)
    lineChart.setTitle("Line Chart Title")
    //defining a series

    val seriesData = LineChartSample.seriesData

    //populating the series with data
    val series = new XYChart.Series[Number, Number]
    series.setName("Series")
    seriesData.map { case (x, y) => new XYChart.Data[Number, Number](x, y) }.foreach(series.getData.add(_))

    val vector = DenseVector(seriesData.map(_._2.doubleValue()): _*)

    val average = sum(vector) / vector.length

    val averageData = for (x <- 1 to seriesData.length) yield (x, average)

    val averageVector = DenseVector(averageData.map(_._2): _*)

    val seriesDataMirror = MirroredSyntheticDataType.findMirror(vector, averageVector).toArray.toList

    println(vector)
    println(averageVector)
    println(seriesDataMirror)

    val seriesMirror = new XYChart.Series[Number, Number]
    seriesMirror.setName("Series Mirror")
    seriesDataMirror
      .zip(seriesData.map(_._1))
      .map { case (y, x) => new XYChart.Data[Number, Number](x, y) }
      .foreach(seriesMirror.getData.add(_))

    val seriesAverage = new XYChart.Series[Number, Number]
    seriesAverage.setName("Series Average")
    averageVector.toArray
      .zip(seriesData.map(_._1))
      .map { case (y, x) => new XYChart.Data[Number, Number](x, y) }
      .foreach(seriesAverage.getData.add(_))

    val scene = new Scene(lineChart, 800, 600)
    lineChart.getData.add(series)
    lineChart.getData.add(seriesMirror)
    lineChart.getData.add(seriesAverage)

    stage.setScene(scene)
    stage.show()
  }

}

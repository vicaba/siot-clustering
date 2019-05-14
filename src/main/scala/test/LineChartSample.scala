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

  def start(data: List[(String, List[(Number, Number)])] = seriesData): Unit = {
    this.seriesData = data
    Application.launch(classOf[LineChartSample])
  }

  private var seriesData: List[(String, List[(Number, Number)])] = _

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
    val seriesData = Option(LineChartSample.seriesData)

    seriesData.map(_.map { s =>
      val seriesChart = new XYChart.Series[Number, Number]
      seriesChart.setName(s._1)
      s._2.map { case (x, y) => new XYChart.Data[Number, Number](x, y) }.foreach(seriesChart.getData.add(_))
      lineChart.getData.add(seriesChart)
    })

    /*
    val vector = DenseVector(seriesData1.map(_._2.doubleValue()): _*)

    val average = sum(vector) / vector.length

    val averageData = for (x <- 1 to seriesData1.length) yield (x, average)

    val averageVector = DenseVector(averageData.map(_._2): _*)

    val seriesDataMirror = MirroredSyntheticDataType.findMirror(vector, averageVector).toArray.toList

    println(vector)
    println(averageVector)
    println(seriesDataMirror)

    val seriesMirror = new XYChart.Series[Number, Number]
    seriesMirror.setName("Series Mirror")
    seriesDataMirror
      .zip(seriesData1.map(_._1))
      .map { case (y, x) => new XYChart.Data[Number, Number](x, y) }
      .foreach(seriesMirror.getData.add(_))

    val seriesAverage = new XYChart.Series[Number, Number]
    seriesAverage.setName("Series Average")
    averageVector.toArray
      .zip(seriesData1.map(_._1))
      .map { case (y, x) => new XYChart.Data[Number, Number](x, y) }
      .foreach(seriesAverage.getData.add(_))

    lineChart.getData.add(seriesMirror)
    lineChart.getData.add(seriesAverage)*/

    val scene = new Scene(lineChart, 800, 600)
    stage.setScene(scene)
    stage.show()
  }

}

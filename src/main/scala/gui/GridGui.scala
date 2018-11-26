package gui


import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.Scene
import breeze.linalg._
import scalafx.scene.control.Button
import scalafx.scene.layout.{BorderPane, HBox, Pane}
import types._
import types.mutable.Cluster
import utils.Generator

import scala.util.Random

/**
  * Example adapted from code showed in [[http://docs.oracle.com/javafx/2/canvas/jfxpub-canvas.htm]].
  */
object GridGui extends JFXApp {

  val width = 1050
  val height = 1050

  val canvas = new Canvas(width, height)

  val forwardButton = new Button("forward")
  val backwardButton = new Button("backwards")

  val menu = new HBox {
    children = Seq(
      forwardButton,
      backwardButton
    )
  }

  val canvasWrapperPane = new Pane {
    children = canvas
  }

  val borderPane = new BorderPane {
    top = menu
    center = canvasWrapperPane
  }

  val gc = canvas.graphicsContext2D

  drawGrid()

  stage = new PrimaryStage {
    title = "Drawing Operations Test"
    scene = new Scene {
      content = borderPane
    }
  }


  def clearCanvas(): Unit = gc.clearRect(0, 0, width, height)


  def drawGrid(): Unit = {
    gc.stroke = Color.Black.opacity(0.2)
    gc.lineWidth = 1
    for (i <- 0 to width by 25) {
      gc.strokeLine(i, 0, i, height)
    }

    gc.lineWidth = 1
    for (j <- 0 to height by 25) {
      gc.strokeLine(0, j, width, j)
    }
  }

  def drawPoint(pointColor: Paint, radius: Double = 10, x: Double, y: Double, text: String): Unit = {
    gc.fill = pointColor
    gc.fillOval(x - radius/2, y - radius/2, radius, radius)
    gc.stroke = Color.Black.opacity(1)
    gc.strokeText(text, x + 10, y + 10)
  }

  def drawCluster(c: Cluster): Unit = {
    val color = Color.rgb(Random.nextInt(255), Random.nextInt(255), Random.nextInt(255), opacity = 1)
    gc.stroke = color
    gc.fill = color
    drawX(c.centroid(0), c.centroid(1), 10)
    c.points.foreach { p =>
      drawPoint(color,10,  p.syntheticValue(0), p.syntheticValue(1), s"(${c.name}, ${p.id})")
    }
  }

  def drawX(x: Double, y: Double, size: Double): Unit = {
    gc.strokeLine(x - size, y, x + size, y)
    gc.strokeLine(x, y - size, x, y + size)
  }

  def drawClusters(clusters: List[Cluster]): Unit = {
    clearCanvas()
    drawGrid()
    clusters.foreach(drawCluster)
  }

}
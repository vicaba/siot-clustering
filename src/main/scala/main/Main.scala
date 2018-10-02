package main
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import algorithm.clusterer.EuclideanClusterer
import breeze.linalg.DenseVector
import eventmanager.Events.NewClusters
import eventmanager.{EventManager, Subscriber}
import gui.{GridGui, GridGuiActor}
import javafx.embed.swing.JFXPanel
import javafx.event.{ActionEvent, EventHandler}
import metrics.Metric
import types.{Cluster, Point, Types2}
import utils.Generator

case class AlgorithmActor(gui: ActorRef) extends Actor {

  var eventStack: List[List[Cluster]] = List()

  var forwardStack: List[List[Cluster]] = List()

  override def preStart(): Unit = {
    super.preStart()
    GridGui.forwardButton.onAction = (event: ActionEvent) => {
      if (forwardStack.nonEmpty) {
        println(eventStack.size)
        val current = forwardStack.head
        forwardStack = forwardStack.drop(1)
        eventStack = current :: eventStack
        gui ! NewClusters(current)
      }

    }

    GridGui.backwardButton.onAction = (event: ActionEvent) => {
      if (eventStack.nonEmpty) {
        println(eventStack.size)
        val current = eventStack.head
        eventStack = eventStack.drop(1)
        forwardStack = current :: forwardStack
        gui ! NewClusters(current)
      }

    }
  }

  override def receive: Receive = {
    case "start" => start()
  }

  def start(): Unit = {

    EventManager.singleton.subscribe(
      "clusters",
      (topic: String, event: Object) => {
        val newClusters = event.asInstanceOf[List[Cluster]]
        eventStack = newClusters :: eventStack
        gui ! NewClusters(newClusters)
      }
    )

    val points = Generator
      .generateRandom2DPoints(DenseVector(500, 500), 500, 50, 5)
      .zipWithIndex
      .map {
        case (m, idx) =>
          Point(idx, m.toDenseVector.asDenseMatrix, None)(Types2)
      }
      .toVector

    println(points)

    EuclideanClusterer.apply(EuclideanClusterer.Settings(2, points, Metric.par))

  }

}

object Main {

  def main(args: Array[String]): Unit = {
    new JFXPanel(); // trick: create empty panel to initialize toolkit
    new Thread(new Runnable() {
      override def run(): Unit = {
        GridGui.main(Array[String]())
      }
    }).start()

    val system         = ActorSystem("Scalatrix")
    val gridGuiActor   = system.actorOf(Props[GridGuiActor], "gridGui-actor")
    val algorithmActor = system.actorOf(Props(AlgorithmActor(gridGuiActor)), "algorithm-actor")
    algorithmActor ! "start"
  }

}

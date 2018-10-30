package main
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import algorithm.clusterer.FlattenedEuclideanClusterer
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

  var events: Vector[List[Cluster]] = Vector()

  var counter: Int = 0

  override def preStart(): Unit = {
    super.preStart()
    GridGui.forwardButton.onAction = (event: ActionEvent) => {
      if (counter > 0) {
        counter = counter - 1
        println(s"Counter: $counter")
        val res = events(counter)
        gui ! NewClusters(events(counter))
      }
    }

    GridGui.backwardButton.onAction = (event: ActionEvent) => {
      if (counter < events.size - 1) {
        counter = counter + 1
        println(s"Counter: $counter")
        val res = events(counter)
        gui ! NewClusters(events(counter))
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
        events = newClusters +: events
        println(s"Events Size:${events.size}")
        gui ! NewClusters(newClusters)
      }
    )

    val points = Generator
      .generateRandom2DPoints(DenseVector(500, 500), 500, 43, 5)
      .zipWithIndex
      .map {
        case (m, idx) =>
          Point(idx, m.toDenseVector.asDenseMatrix, None)(Types2)
      }
      .toVector

    println(points)

    FlattenedEuclideanClusterer.applyOnce(FlattenedEuclideanClusterer.Settings(4, points, Metric.par))

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

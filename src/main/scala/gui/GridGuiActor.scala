package gui
import akka.actor.Actor
import eventmanager.Events.NewClusters

class GridGuiActor extends Actor {
  override def receive: Receive = {
    case NewClusters(clusters) => {
      println("new")
      GridGui.drawClusters(clusters)
    }
  }
}

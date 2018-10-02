package eventmanager
import types.Cluster

object Events {
  case class NewClusters(clusters: List[Cluster])
}

object EventManager {

  val singleton: EventManager = new EventManager

}

trait Subscriber {
  def onEvent(topic: String, event: Object)
}

class EventManager {

  private val topicToSubscribers: scala.collection.mutable.Map[String, List[Subscriber]] =
    scala.collection.mutable.Map[String, List[Subscriber]]()

  def subscribe(topic: String, subscriber: Subscriber): Unit = {
    val subscribers = topicToSubscribers.applyOrElse(topic, { t: String =>
      List.empty[Subscriber]
    })
    println("subscribed to topic")
    topicToSubscribers.+=(topic -> (subscriber +: subscribers))
  }

  def publish(topic: String, event: Object): Unit =
    topicToSubscribers.get(topic).foreach(_.foreach(_.onEvent(topic, event)))

}

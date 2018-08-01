package eventmanager

object EventManager {

  val singleton: EventManager = new EventManager

}

trait Subscriber {
  def onEvent(topic: String, event: Object)
}

class EventManager {

  val topicToSubscribers: scala.collection.mutable.Map[String, List[Subscriber]] =
    scala.collection.mutable.Map[String, List[Subscriber]]()

  def subscribe(topic: String, subscriber: Subscriber): Unit = {
    val subscribers = topicToSubscribers.applyOrElse(topic, { t: String =>
      List.empty[Subscriber]
    })
    topicToSubscribers.+=(topic -> (subscriber +: subscribers))
  }

  def publish(topic: String, event: Object): Unit =
    topicToSubscribers.get(topic).foreach(_.foreach(_.onEvent(topic, event)))

}

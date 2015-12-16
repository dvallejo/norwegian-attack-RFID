package com.scalera.rfid

import akka.actor.{ ActorSystem, ActorRef }
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent._

object Main extends App {

  println("Starting...")

  val system = ActorSystem("my-first-system")

  val tagActor: ActorRef =
    system.actorOf(TagActor.props("ID1"), "Tag")

  val manInTheMiddleActor: ActorRef =
    system.actorOf(ManInTheMiddleActor.props(tagActor), "Man-In-The_Middle")

  val readerActor: ActorRef =
    system.actorOf(ReaderActor.props(manInTheMiddleActor), "Reader")

  implicit val timeout = Timeout(1 seconds)

  val n = 10

  val results =
    (1 to n).foldLeft(List.empty[String]) {
      case (l, _) =>
        val response = (readerActor ? Messages.Start).mapTo[String]
        val value = Await.result(response, 1 seconds)
        l :+ value
    }

  println("FINISH!")
  println(results)
  system.shutdown

}

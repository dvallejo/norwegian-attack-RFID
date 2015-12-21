package com.scalera.rfid

import akka.actor.{ ActorSystem, ActorRef }
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent._
import scala.util._

import SASIUtils._

object Main extends App {

  println("Starting...")

  val system = ActorSystem("my-first-system")

  val id = generateRandomValue
  val ids = generateRandomValue
  val k1 = generateRandomValue
  val k2 = generateRandomValue

  println("id = " + id.toBin)

  val tagActor: ActorRef =
    system.actorOf(TagActor.props(ids, k1, k2, id), "Tag")

  val manInTheMiddleActor: ActorRef =
    system.actorOf(ManInTheMiddleActor.props(tagActor), "Man-In-The_Middle")

  val readerActor: ActorRef =
    system.actorOf(ReaderActor.props(ids, k1, k2, id, manInTheMiddleActor), "Reader")

  implicit val timeout = Timeout(1 seconds)

  val n = 10000

  val results =
    (1 to n).foldLeft(List.empty[String]) {
      case (l, _) =>
        val response = (readerActor ? Messages.Start).mapTo[String]
        val value = Await.result(response, 1 seconds)
        l :+ value
    }

  println("FINISH!")

  val response = (manInTheMiddleActor ? Messages.GetResult).mapTo[Map[String, Int]]
  
  response.onComplete{ 
    case Success(map) =>
      val (maxValue, times) = map.toList.sortBy(_._2).last
      println(s"Max probability value is $maxValue. Found $times times")
      system.shutdown
    case Failure(_: Throwable) =>
      system.shutdown
  }

}

package com.scalera.rfid.yehetal

import akka.actor.{ ActorSystem, ActorRef }
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent._
import scala.util._

import YehEtAlUtils._

object YehEtAlMain extends App {

  println("Starting...")

  val system = ActorSystem("my-first-system")

  val id = generateRandomValue
  val ids = generateRandomValue
  val idsOld = generateRandomValue
  val k = generateRandomValue

  println("id = " + id.toBin)

  val tagActor: ActorRef =
    system.actorOf(TagActor.props(ids, k, id), "Tag")

  val manInTheMiddleActor: ActorRef =
    system.actorOf(ManInTheMiddleActor.props(tagActor), "Man-In-The_Middle")

  val readerActor: ActorRef =
    system.actorOf(ReaderActor.props(ids, idsOld, k, id, manInTheMiddleActor), "Reader")

  implicit val timeout = Timeout(10 seconds)

  val n = 100000

  val results =
    (1 to n).foldLeft(List.empty[String]) {
      case (l, _) =>
        val response = (readerActor ? Messages.Start).mapTo[String]
        val value = Await.result(response, 10 seconds)
        l :+ value
    }

  println("FINISH!")

  val response = (manInTheMiddleActor ? Messages.GetResult).mapTo[Map[String, Int]]
  
  response.onComplete{ 
    case Success(map) if map.nonEmpty =>
      val (maxValue, times) = map.toList.sortBy(_._2).last
      println(s"Max probability value is $maxValue. Found $times times")
      val (maxValue2, times2) = map.toList.sortBy(_._2).init.last
      println(s"Max probability value is $maxValue2. Found $times2 times")
      val (maxValue3, times3) = map.toList.sortBy(_._2).init.init.last
      println(s"Max probability value is $maxValue3. Found $times3 times")
      system.shutdown
    case _ =>
      system.shutdown
  }

}

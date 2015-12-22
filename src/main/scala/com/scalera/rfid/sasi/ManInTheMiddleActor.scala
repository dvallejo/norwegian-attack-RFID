package com.scalera.rfid.sasi

import akka.actor.{ Actor, ActorRef, Props }
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scodec.bits._

import SASIUtils._
import Messages._

class ManInTheMiddleActor(tag: ActorRef) extends Actor {

  implicit val timeout = Timeout(1 seconds)

  var reader: Option[ActorRef] = None

  var ids = BitVector(new Array[Byte](size / 8))
  var vulnerabilityDetected = false
  var lastIds = BitVector(new Array[Byte](size / 8))
  var idMap: Map[String, Int] = Map.empty[String, Int]

  def receive = {

    case Hello =>      
      reader = Option(sender)
      tag ! Hello

    case idsMessage @ IDS(idsContent) =>
      ids = idsContent
      if(vulnerabilityDetected) {
        val idEstimated = estimateId(idsContent, lastIds)
        vulnerabilityDetected = false
        idMap = idMap + (idEstimated.toBin -> (idMap.get(idEstimated.toBin).getOrElse(0) + 1))
      }
      reader.get ! idsMessage

    case abc @ ABC(A(a), B(b), C(c)) =>
      if(isVulnerable(c, a, ids, b)) {
        // println("Vulnerability Detected!")
        vulnerabilityDetected = true
      }
      tag ! abc

    case d: D =>
      lastIds = ids
      reader.get ! d

    case GetResult =>
      sender ! idMap
  }
}

object ManInTheMiddleActor {

  def props(tag: ActorRef): Props =
    Props(new ManInTheMiddleActor(tag))

}
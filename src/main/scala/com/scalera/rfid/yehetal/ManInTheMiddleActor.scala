package com.scalera.rfid.yehetal

import akka.actor.{ Actor, ActorRef, Props }
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scodec.bits._

import YehEtAlUtils._
import Messages._

class ManInTheMiddleActor(tag: ActorRef) extends Actor {

  implicit val timeout = Timeout(1 seconds)

  var reader: Option[ActorRef] = None

  var ids = BitVector(Array.empty[Byte])
  var c = BitVector(Array.empty[Byte])
  var d = BitVector(Array.empty[Byte])
  
  var vulnerabilityDetected = false
  var idMap: Map[String, Int] = Map.empty[String, Int]

  def receive = {

    case Hello =>      
      reader = Option(sender)
      tag ! Hello

    case idsMessage @ IDS(idsContent) =>
      
      if(vulnerabilityDetected) {
        val idEstimated = estimateId(idsContent, ids, d)
        vulnerabilityDetected = false
        idMap = idMap + 
          (
            idEstimated.takeRight(5).toBin ->
            (idMap.get(idEstimated.takeRight(5).toBin).getOrElse(0) + 1)
          )
      }
      
      ids = idsContent
      reader.get ! idsMessage

    case abc @ ABC(_, _, C(cContent), _) =>
      c = cContent
      tag ! abc

    case dMessage @ D(dContent) =>
      
      d = dContent

      if(isVulnerable(c, d))
        vulnerabilityDetected = true

      reader.get ! dMessage

    case GetResult =>
      sender ! idMap
  }
}

object ManInTheMiddleActor {

  def props(tag: ActorRef): Props =
    Props(new ManInTheMiddleActor(tag))

}
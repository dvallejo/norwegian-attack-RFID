package com.scalera.rfid

import akka.actor.{ Actor, ActorRef, Props }
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import Messages._

class ManInTheMiddleActor(tag: ActorRef) extends Actor {

  implicit val timeout = Timeout(1 seconds)

  var reader: Option[ActorRef] = None

  def receive = {

    case Hello =>      
      reader = Option(sender)
      tag ! Hello

    case ids: IDS =>
      reader.get ! ids

    case abc: ABC =>
      tag ! abc

    case d: D =>
      reader.get ! d
  }
}

object ManInTheMiddleActor {

  def props(tag: ActorRef): Props =
    Props(new ManInTheMiddleActor(tag))

}
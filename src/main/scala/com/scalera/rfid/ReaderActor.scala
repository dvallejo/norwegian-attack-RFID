package com.scalera.rfid

import akka.actor.{ Actor, ActorRef, Props }

import Messages._

class ReaderActor(tag: ActorRef) extends Actor {

  var mainRefOpt: Option[ActorRef] = None

  def receive = {
    case Start =>
      mainRefOpt = Option(sender)
      println("Received START")
      tag ! Hello
    
    case IDS(content) =>
      println("Received IDS")
      sender ! ABC(A(content), B(content), C(content))

    case D(content) =>
      println("Received D")
      mainRefOpt match {
        case Some(mainRef) => mainRef ! new String(content)
        case None => println(new String(content))
      }
  }
}

object ReaderActor {

  def props(tag: ActorRef): Props = Props(new ReaderActor(tag))

}

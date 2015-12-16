package com.scalera.rfid

import akka.actor.{ Actor, Props }

import Messages._

class TagActor(_id: String) extends Actor {

  var id = _id

  def receive = {
    case Hello =>
      println("Received Hello")
      sender ! IDS(id.getBytes)

    case ABC(A(aContent), B(bContent), C(cContent)) =>
      println("Received A|B|C")
      sender ! D(aContent)
  }
}

object TagActor {

  def props(id: String): Props = Props(new TagActor(id))

}

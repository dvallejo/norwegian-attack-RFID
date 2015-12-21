package com.scalera.rfid

import akka.actor.{ Actor, ActorRef, Props }
import scodec.bits._

import SASIUtils._
import Messages._

class ReaderActor(_ids: BitVector, _k1: BitVector, _k2: BitVector, _id: BitVector, tag: ActorRef) extends Actor {

  var mainRefOpt: Option[ActorRef] = None
  val id = _id
  var ids = _ids
  var k1 = _k1
  var k2 = _k2
  var n1 = BitVector(new Array[Byte](size / 8))
  var n2 = BitVector(new Array[Byte](size / 8)) 
  var k1Hat = createK1Hat(k1, n2)
  var k2Hat = createK2Hat(k2, n1)

  def receive = {
    case Start =>
      mainRefOpt = Option(sender)
      tag ! Hello
    
    case IDS(ids) =>
      n1 = generateRandomValue
      n2 = generateRandomValue
      k1Hat = createK1Hat(k1, n2)
      k2Hat = createK2Hat(k2, n1)
      val aContent = createA(ids, k1, n1)
      val bContent = createB(ids, k2, n2)
      val cContent = createC(k1, k2, k1Hat, k2Hat)
      sender ! ABC(A(aContent), B(bContent), C(cContent))

    case D(dContent) =>
      //Check
      val check = createD(k2Hat, id, k1, k2, k1Hat) == dContent
      if(!check)
        println("Error checking D")
      mainRefOpt match {
        case Some(mainRef) => mainRef ! dContent.toBin
        case None => println(dContent.toBin)
      }
      ids = createNewIDS(ids, id, n2, k1Hat)
      k1 = k1Hat
      k2 = k2Hat
  }
}

object ReaderActor {

  def props(ids: BitVector, k1: BitVector, k2: BitVector, id: BitVector, tag: ActorRef): Props =
    Props(new ReaderActor(ids, k1, k2, id, tag))

}

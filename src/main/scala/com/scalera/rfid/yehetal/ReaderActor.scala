package com.scalera.rfid.yehetal

import akka.actor.{ Actor, ActorRef, Props }
import scodec.bits._

import YehEtAlUtils._
import Messages._

class ReaderActor(_idsNew: BitVector, _idsOld: BitVector, _k: BitVector, _id: BitVector, tag: ActorRef) extends Actor {

  var mainRefOpt: Option[ActorRef] = None
  val id = _id
  var ids = BitVector(Array.empty[Byte])
  var idsNew = _idsNew
  var idsOld = _idsOld
  var k = _k
  var n1 = BitVector(Array.empty[Byte])
  var n2 = BitVector(Array.empty[Byte]) 
  var kHat = createKHat(k, n1, n2)
  var flag = 0

  def receive = {
    case Start =>
      tag ! createHello(sender)
    
    case IDS(idsContent) =>
      sender ! createABC(idsContent)

    case D(dContent) =>
      checkResponse(dContent)
      
  }

  def createHello(sender: ActorRef) = {
    mainRefOpt = Option(sender)
    Hello
  }

  def createABC(idsContent: BitVector) = {
    ids = idsContent
    if(ids.toLong() == idsNew.toLong())
      flag = 0
    else {
      k = id
      flag = 1
    }

    n1 = generateRandomValue
    n2 = generateRandomValue

    kHat = createKHat(k, n1, n2)
    val aContent = createA(ids, k, n1)
    val bContent = createB(ids, k, n2)
    val cContent = createC(kHat, n1, n2)

    ABC(A(aContent), B(bContent), C(cContent), flag)
  }

  def checkResponse(dContent: BitVector) = {
    
    val k1Hat = createK1Hat(k, n1, n2)
    idsOld = ids
    idsNew = createNewIDS(ids, id, n1, n2, k1Hat)
    k = kHat
    
    if(createD(k1Hat, n1, n2) != dContent)
      println("Error checking D")
    
    mainRefOpt match {
      case Some(mainRef) => mainRef ! dContent.toBin
      case None => println(dContent.toBin)
    }
  }
}

object ReaderActor {

  def props(idsNew: BitVector, idsOld: BitVector, k: BitVector, id: BitVector, tag: ActorRef): Props =
    Props(new ReaderActor(idsNew, idsOld, k, id, tag))

}

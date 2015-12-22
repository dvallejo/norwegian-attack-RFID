package com.scalera.rfid.yehetal

import akka.actor.{ Actor, ActorRef, Props }
import scodec.bits._

import YehEtAlUtils._
import Messages._

class ReaderActor(_idsNew: BitVector, _idsOld: BitVector, _k: BitVector, _id: BitVector, tag: ActorRef) extends Actor {

  var mainRefOpt: Option[ActorRef] = None
  val id = _id
  var ids = BitVector(new Array[Byte](size / 8))
  var idsNew = _idsNew
  var idsOld = _idsOld
  var k = _k
  var n1 = BitVector(new Array[Byte](size / 8))
  var n2 = BitVector(new Array[Byte](size / 8)) 
  var kHat = createKHat(k, n1, n2)
  var flag = 0

  def receive = {
    case Start =>
      mainRefOpt = Option(sender)
      tag ! Hello
    
    case IDS(idsContent) =>
      ids = idsContent
      if(ids.toLong() == idsNew.toLong()) {
        flag = 0
      } else {
        k = id
        flag = 1
      }

      n1 = generateRandomValue
      n2 = generateRandomValue

      kHat = createKHat(k, n1, n2)
      val aContent = createA(ids, k, n1)
      val bContent = createB(ids, k, n2)
      val cContent = createC(kHat, n1, n2)

      sender ! ABC(A(aContent), B(bContent), C(cContent), flag)

    case D(dContent) =>
      //Check
      val k1Hat = createK1Hat(k, n1, n2)
      val check = createD(k1Hat, n1, n2) == dContent

      idsOld = ids

      idsNew = createNewIDS(ids, id, n1, n2, k1Hat)

      k = kHat
      
      if(!check)
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

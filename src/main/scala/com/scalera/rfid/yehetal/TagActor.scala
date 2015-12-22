package com.scalera.rfid.yehetal

import akka.actor.{ Actor, Props }
import scodec.bits._

import YehEtAlUtils._
import Messages._

class TagActor(_ids: BitVector, _k: BitVector, _id: BitVector) extends Actor {

  var id = _id
  var ids = _ids
  var k = _k
  var n1 = BitVector(new Array[Byte](size / 8))
  var n2 = BitVector(new Array[Byte](size / 8)) 
  var kHat = BitVector(new Array[Byte](size / 8)) 
  var k1Hat = createK1Hat(k, n1, n2)

  def receive = {
    case Hello =>
      sender ! IDS(ids)

    case ABC(A(aContent), B(bContent), C(cContent), flag) =>
      if(flag == 1)
        k = id

      n1 = getn1(aContent, ids, k)
      n2 = getn2(bContent, ids, k)
      kHat = createKHat(k, n1, n2)
      
      // //Check
      val check = createC(kHat, n1, n2) == cContent
      if(!check)
        println("Error checking C")

      val k1Hat = createK1Hat(k, n1, n2)
      val dContent = createD(k1Hat, n1, n2)

      val newIds = createNewIDS(ids, id, n1, n2, k1Hat)

      ids = newIds.copy
      
      k = kHat
      sender ! D(dContent)

  }
}

object TagActor {

  def props(ids: BitVector, k: BitVector, id: BitVector): Props =
    Props(new TagActor(ids, k, id))

}

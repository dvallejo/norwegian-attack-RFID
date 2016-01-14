package com.scalera.rfid.yehetal

import akka.actor.{ Actor, Props }
import scodec.bits._

import YehEtAlUtils._
import Messages._

class TagActor(_ids: BitVector, _k: BitVector, _id: BitVector) extends Actor {

  var id = _id
  var ids = _ids
  var k = _k
  var n1 = BitVector(Array.empty[Byte])
  var n2 = BitVector(Array.empty[Byte]) 
  var kHat = BitVector(Array.empty[Byte]) 
  var k1Hat = createK1Hat(k, n1, n2)

  def receive = {
    case Hello =>
      sender ! IDS(ids)

    case ABC(A(aContent), B(bContent), C(cContent), flag) =>
      sender ! checkResponse(aContent, bContent, cContent, flag)
  }

  def checkResponse(a: BitVector, b: BitVector, c: BitVector, flag: Int) = {
    
    if(flag == 1)
      k = id

    n1 = getn1(a, ids, k)
    n2 = getn2(b, ids, k)
    kHat = createKHat(k, n1, n2)
    
    if(createC(kHat, n1, n2) != c)
      println("Error checking C")

    val k1Hat = createK1Hat(k, n1, n2)
    val dContent = createD(k1Hat, n1, n2)

    val newIds = createNewIDS(ids, id, n1, n2, k1Hat)

    ids = newIds.copy
    
    k = kHat

    D(dContent)
  }
}

object TagActor {

  def props(ids: BitVector, k: BitVector, id: BitVector): Props =
    Props(new TagActor(ids, k, id))

}

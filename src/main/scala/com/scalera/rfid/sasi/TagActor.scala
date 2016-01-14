package com.scalera.rfid.sasi

import akka.actor.{ Actor, Props }
import scodec.bits._

import SASIUtils._
import Messages._

class TagActor(_ids: BitVector, _k1: BitVector, _k2: BitVector, _id: BitVector) extends Actor {

  var id = _id
  var ids = _ids
  var k1 = _k1
  var k2 = _k2
  var n1 = BitVector(Array.empty[Byte])
  var n2 = BitVector(Array.empty[Byte]) 
  var k1Hat = createK1Hat(k1, n2)
  var k2Hat = createK2Hat(k2, n1)

  def receive = {
    case Hello =>
      sender ! IDS(ids)

    case ABC(A(aContent), B(bContent), C(cContent)) =>
      sender ! checkResponse(aContent, bContent, cContent)
      ids = createNewIDS(ids, id, n2, k1Hat)
      k1 = k1Hat
      k2 = k2Hat
  }

  def checkResponse(a: BitVector, b: BitVector, c: BitVector) = {

    n1 = getn1(a, ids, k1)
    n2 = getn2(b, ids, k2)
    k1Hat = createK1Hat(k1, n2)
    k2Hat = createK2Hat(k2, n1)

    if(createC(k1, k2, k1Hat, k2Hat) != c)
      println("Error checking C")
    
    D(createD(k2Hat, id, k1, k2, k1Hat))
  }
}

object TagActor {

  def props(ids: BitVector, k1: BitVector, k2: BitVector,id: BitVector): Props =
    Props(new TagActor(ids, k1, k2, id))

}

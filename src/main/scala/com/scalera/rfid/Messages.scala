package com.scalera.rfid

import akka.actor.ActorRef

object Messages {

  case object Start
  case object Hello
  case class IDS(content: Array[Byte])
  case class A(content: Array[Byte])
  case class B(content: Array[Byte])
  case class C(content: Array[Byte])
  case class ABC(a: A, b: B, c: C)
  case class D(content: Array[Byte])

}

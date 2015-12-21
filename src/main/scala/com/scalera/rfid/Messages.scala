package com.scalera.rfid

import scodec.bits._

object Messages {

  case object Start
  case object Hello
  case object GetResult
  case class IDS(content: BitVector)
  case class A(content: BitVector)
  case class B(content: BitVector)
  case class C(content: BitVector)
  case class ABC(a: A, b: B, c: C)
  case class D(content: BitVector)

}

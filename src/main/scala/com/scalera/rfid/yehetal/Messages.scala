package com.scalera.rfid.yehetal

import scodec.bits._

object Messages {

  case object Start
  case object Hello
  case object GetResult
  case class IDS(content: BitVector)
  case class A(content: BitVector)
  case class B(content: BitVector)
  case class C(content: BitVector)
  case class ABC(a: A, b: B, c: C, flag: Int)
  case class D(content: BitVector)

}

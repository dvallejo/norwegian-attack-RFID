package com.scalera.rfid.utils

import scodec.bits._

trait BitUtils {

  val size: Int

  implicit class MyBitVector(myVector: BitVector) {

    def +(other: BitVector) = {
      val newBitVector =
        BitVector.fromInt((myVector.toInt() + other.toInt()) % Math.pow(2.0, size).toInt)
      
      newBitVector.drop(newBitVector.size - size)
    }

    def -(other: BitVector) = {
      val newBitVector =
        BitVector.fromInt((myVector.toInt() - other.toInt()) % Math.pow(2.0, size).toInt)
      
      newBitVector.drop(newBitVector.size - size)
    }

    def <<(other: BitVector) =
     myVector << (other.toInt() % Math.pow(2.0, size).toInt)

  }
}
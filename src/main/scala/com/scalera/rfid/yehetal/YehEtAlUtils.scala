package com.scalera.rfid.yehetal

import scala.util.Random
import scodec.bits._

object YehEtAlUtils {

  val size: Int = 64 //8 Bytes

  def generateRandomValue: BitVector =
    BitVector.fromLong(Random.nextInt, size)

  def createA(ids: BitVector, k: BitVector, n1: BitVector): BitVector =
    ids ^ k ^ n1
  
  def createB(ids: BitVector, k: BitVector, n2: BitVector): BitVector =
    (ids | k) ^ n2
  
  def createC(kHat: BitVector, n1: BitVector, n2: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong(((kHat ^ n1).toLong() + n2.toLong()) % Math.pow(2.0, size).toLong)
    newBitVector.drop(newBitVector.size - size)
  }

  def createKHat(k: BitVector, n1: BitVector, n2: BitVector): BitVector =
    (k ^ n2) << n1.toLong()

  def createK1Hat(k: BitVector, n1: BitVector, n2: BitVector): BitVector =
    (k ^ n1) << n2.toLong()

  def createD(k1Hat: BitVector, n1: BitVector, n2: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong((k1Hat ^ n2).toLong() + n1.toLong() % Math.pow(2.0, size).toLong)
    newBitVector.drop(newBitVector.size - size)
  }

  def getn1(a: BitVector, ids: BitVector, k: BitVector): BitVector =
    ids ^ k ^ a

  def getn2(b: BitVector, ids: BitVector, k: BitVector): BitVector =
    (ids | k) ^ b

  def createNewIDS(ids: BitVector, id: BitVector, n1: BitVector, n2: BitVector, k1Hat: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong(ids.toLong() + (id ^ k1Hat).toLong() % Math.pow(2.0, size).toLong) ^ n1 ^ n2
    newBitVector.drop(newBitVector.size - size)
  }

  def isVulnerable(c: BitVector, d: BitVector): Boolean =
    c.toLong() % size == d.toLong() % size

  def estimateId(idsNext: BitVector, ids: BitVector, d: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong((idsNext.toLong() - ids.toLong()) % Math.pow(2.0, size).toLong)
    val result = newBitVector.drop(newBitVector.size - size) ^ d
    val r = BitVector.fromLong(result.toLong() % size)
    r.drop(r.size - size)
  }
}

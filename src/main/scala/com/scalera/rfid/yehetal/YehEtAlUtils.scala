package com.scalera.rfid.yehetal

import com.scalera.rfid.utils.BitUtils

import scala.util.Random
import scodec.bits._

object YehEtAlUtils extends BitUtils {

  val size: Int = 32 //4 Bytes

  def generateRandomValue: BitVector =
    BitVector.fromInt(Random.nextInt, size)

  def createA(ids: BitVector, k: BitVector, n1: BitVector): BitVector =
    ids ^ k ^ n1
  
  def createB(ids: BitVector, k: BitVector, n2: BitVector): BitVector =
    (ids or k) ^ n2
  
  def createC(kHat: BitVector, n1: BitVector, n2: BitVector): BitVector =
    (kHat ^ n1) + n2

  def createKHat(k: BitVector, n1: BitVector, n2: BitVector): BitVector =
    (k ^ n2) << n1

  def createK1Hat(k: BitVector, n1: BitVector, n2: BitVector): BitVector =
    (k ^ n1) << n2

  def createD(k1Hat: BitVector, n1: BitVector, n2: BitVector): BitVector =
    (k1Hat ^ n2) + n1

  def getn1(a: BitVector, ids: BitVector, k: BitVector): BitVector =
    ids ^ k ^ a

  def getn2(b: BitVector, ids: BitVector, k: BitVector): BitVector =
    (ids or k) ^ b

  def createNewIDS(ids: BitVector, id: BitVector, n1: BitVector, n2: BitVector, k1Hat: BitVector): BitVector =
    (ids + (id ^ k1Hat)) ^ n1 ^ n2

  def isVulnerable(c: BitVector, d: BitVector): Boolean =
    c.toInt() % size == d.toInt() % size

  def estimateId(idsNext: BitVector, ids: BitVector, d: BitVector): BitVector = {
    val result = (idsNext - ids) ^ d
    val r = BitVector.fromInt(result.toInt() % size)
    r.drop(r.size - size)
  }
}

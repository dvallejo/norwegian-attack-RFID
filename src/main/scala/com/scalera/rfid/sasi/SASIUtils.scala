package com.scalera.rfid.sasi

import com.scalera.rfid.utils.BitUtils

import scala.util.Random
import scodec.bits._

object SASIUtils extends BitUtils {

  val size: Int = 32 //4 Bytes

  def generateRandomValue: BitVector =
    BitVector.fromInt(Random.nextInt, size)

  def createA(ids: BitVector, k1: BitVector, n1: BitVector): BitVector =
    ids ^ k1 ^ n1
  
  def createB(ids: BitVector, k2: BitVector, n2: BitVector): BitVector =
    (ids or k2) + n2
  
  def createC(k1: BitVector, k2: BitVector, k1Hat: BitVector, k2Hat: BitVector): BitVector =
    (k1 ^ k2Hat) + (k2 ^ k1Hat)

  def createD(k2Hat: BitVector, id: BitVector, k1: BitVector, k2: BitVector, k1Hat: BitVector): BitVector =
    (k2Hat + id) ^ ((k1 ^ k2) or k1Hat)

  def createK1Hat(k1: BitVector, n2: BitVector): BitVector =
    (k1 ^ n2) << k1
  
  def createK2Hat(k2: BitVector, n1: BitVector): BitVector =
    (k2 ^ n1) << k2

  def getn1(a: BitVector, ids: BitVector, k1: BitVector): BitVector =
    ids ^ k1 ^ a

  def getn2(b: BitVector, ids: BitVector, k2: BitVector): BitVector =
    b - (ids or k2)

  def createNewIDS(ids: BitVector, id: BitVector, n2: BitVector, k1Hat: BitVector): BitVector =
    (ids + id) ^ (n2 ^ k1Hat)

  def estimateC(c: BitVector, a: BitVector, ids: BitVector, b: BitVector): BitVector = {
    val newBitVector = BitVector.fromInt(((a ^ ids).toInt() + b.toInt() - ids.toInt()) % size)
    newBitVector.drop(newBitVector.size - size)
  }

  def isVulnerable(c: BitVector, a: BitVector, ids: BitVector, b: BitVector): Boolean = {
    c.toInt() % size == estimateC(c, a, ids, b).toInt()
  }

  def estimateId(idsNext: BitVector, ids: BitVector): BitVector = {
    val newBitVector = BitVector.fromInt((idsNext.toInt() - ids.toInt()) % size)
    newBitVector.drop(newBitVector.size - size)
  }
}

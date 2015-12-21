package com.scalera.rfid

import scala.util.Random
import scodec.bits._

object SASIUtils {

  val size: Int = 64 //8 Bytes

  def generateRandomValue: BitVector =
    BitVector.fromLong(Random.nextInt, size)

  def createA(ids: BitVector, k1: BitVector, n1: BitVector): BitVector =
    ids ^ k1 ^ n1
  
  def createB(ids: BitVector, k2: BitVector, n2: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong(((ids or k2).toLong() + n2.toLong()) % Math.pow(2.0, size).toLong)
    newBitVector.drop(newBitVector.size - size)
  }
  
  def createC(k1: BitVector, k2: BitVector, k1Hat: BitVector, k2Hat: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong(((k1 ^ k2Hat).toLong() + (k2 ^ k1Hat).toLong()) % Math.pow(2.0, size).toLong)
    newBitVector.drop(newBitVector.size - size)
  }

  def createD(k2Hat: BitVector, id: BitVector, k1: BitVector, k2: BitVector, k1Hat: BitVector): BitVector =
    BitVector.fromLong((k2Hat.toLong() + id.toLong()) % size) ^ ((k1 ^ k2) | k1Hat)

  def createK1Hat(k1: BitVector, n2: BitVector): BitVector =
    (k1 ^ n2) << k1.toLong()
  
  def createK2Hat(k2: BitVector, n1: BitVector): BitVector =
    (k2 ^ n1) << k2.toLong()

  def getn1(a: BitVector, ids: BitVector, k1: BitVector): BitVector =
    ids ^ k1 ^ a

  def getn2(b: BitVector, ids: BitVector, k2: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong((b.toLong() - (ids or k2).toLong()) % Math.pow(2.0, size).toLong)
    newBitVector.drop(newBitVector.size - size)
  }

  def createNewIDS(ids: BitVector, id: BitVector, n2: BitVector, k1Hat: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong(ids.toLong() + id.toLong()) ^ (n2 ^ k1Hat)
    newBitVector.drop(newBitVector.size - size)
  }

  def estimateC(c: BitVector, a: BitVector, ids: BitVector, b: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong(((a ^ ids).toLong() + b.toLong() - ids.toLong()) % size)
    newBitVector.drop(newBitVector.size - size)
  }

  def isVulnerable(c: BitVector, a: BitVector, ids: BitVector, b: BitVector): Boolean = {
    c.toLong() % size == estimateC(c, a, ids, b).toLong()
  }

  def estimateId(idsNext: BitVector, ids: BitVector): BitVector = {
    val newBitVector = BitVector.fromLong((idsNext.toLong() - ids.toLong()) % size)
    newBitVector.drop(newBitVector.size - size)
  }
}

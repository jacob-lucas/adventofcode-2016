package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 {

  val salt: String = "qzyelonm"

  val keys: mutable.MutableList[String] = mutable.MutableList()

  def md5(str: String, index: Long): String = {
    java.security.MessageDigest.getInstance("MD5")
      .digest((str + index).getBytes())
      .map(0xFF & _)
      .map { "%02x".format(_) }
      .foldLeft(""){_ + _}
  }

  def expandKeyset(): Unit = {
    println("Expanding keyset")
    val index = if (keys.isEmpty) 0 else keys.length + 1
    for {
      i <- index until (index + 1000)
    } {
      keys += md5(salt, i)
    }
  }

  @tailrec
  def isKey(s: String, index: Int): Boolean = {
    s.toList match {
      case x :: y :: z :: tail if s.length >= 3 =>
        if (x == y && x == z) {
          // check if we need to expand
          if (keys.length - index < 1000) expandKeyset()

          // check for 5 in next 1000
          val cands = keys.slice(index, index + 1000)

          @tailrec
          def contains5(ch: Char, str: String): Boolean = {
            if (str.length <= 5) false
            else if (ch == str.charAt(0) && str.charAt(0) == str.charAt(1) && str.charAt(1) == str.charAt(2) && str.charAt(2) == str.charAt(3) && str.charAt(3) == str.charAt(4)) true
            else contains5(ch, str.tail)
          }

          cands.exists(c => contains5(x, c))
        }
        else isKey(s.tail, index)
      case _ => false
    }
  }

  @tailrec
  def findKeys(n: Int, index: Int = 0, acc: List[(String, Int)] = List()): List[(String, Int)] = {
    if (acc.length == n) {
      acc
    } else {
      val hash = md5(salt, index)
      findKeys(n, index + 1, if (isKey(hash, index + 1)) acc :+ (hash, index) else acc)
    }
  }

  def main(args: Array[String]): Unit = {
    expandKeyset()
    val keys = findKeys(64)
    var i = 1
    keys.foreach(k => {
      println(i + ") " + k)
      i = i + 1
    })
  }
}

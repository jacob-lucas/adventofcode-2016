package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

object Day05 {

  def md5(str: String): String = {
    java.security.MessageDigest.getInstance("MD5")
      .digest(str.getBytes())
      .map(0xFF & _)
      .map { "%02x".format(_) }
      .foldLeft(""){_ + _}
  }

  @tailrec
  def findHash(input: String, index: Int): (String, Int) = {
    val hash = md5(input + index)
    if (hash.startsWith("00000")) (hash, index)
    else findHash(input, index + 1)
  }

  def password(doorId: String): String = {
    var index = 0
    Array.fill(8)({
      val (hash, idx) = findHash(doorId, index)
      index = idx + 1
      hash.charAt(5)
    }).mkString
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day05-input.txt")).getLines().toList
    println(password(lines.head))
  }

}

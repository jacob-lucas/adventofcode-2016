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
    def decrypt(n: Int, index: Int, arr: Array[Char]): String = {
      if (n > 7) arr.mkString
      else {
        val (hash, idx) = findHash(doorId, index)
        val insertIndex = hash.charAt(5)
        try {
          val insertIndexInt = insertIndex.asDigit
          if (insertIndexInt < 0 || insertIndexInt > 7) {
            throw new RuntimeException("bad index")
          } else {
            if (arr(insertIndexInt) != '_') {
              throw new RuntimeException("already found char for index")
            }
            arr(insertIndexInt) = hash.charAt(6)
            println(arr.mkString)
            decrypt(n + 1, idx + 1, arr)
          }
        } catch {
          case _: Throwable => decrypt(n, idx + 1, arr)
        }
      }
    }

    decrypt(0, 0, Array.fill(8)('_'))
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day05-input.txt")).getLines().toList
    println(password(lines.head))
  }

}

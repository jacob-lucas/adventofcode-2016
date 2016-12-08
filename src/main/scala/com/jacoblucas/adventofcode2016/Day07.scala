package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

object Day07 {

  @tailrec
  def separate(str: String, acc: String, valid: List[String], invalid: List[String], addingToValid: Boolean): (List[String], List[String]) = {
    if (str.isEmpty && addingToValid) (valid :+ acc, invalid)
    else if (str.isEmpty && !addingToValid) (valid, invalid :+ acc)
    else {
      val ch = str.head
      if (ch != '[' && ch != ']') {
        separate(str.tail, acc :+ ch, valid, invalid, addingToValid)
      } else {
        if (addingToValid)
          separate(str.tail, "", valid :+ acc, invalid, !addingToValid)
        else
          separate(str.tail, "", valid, invalid :+ acc, !addingToValid)
      }
    }
  }

  def isABBA(str: String): Boolean = {
    if (str.length != 4) false
    else str.charAt(0) == str.charAt(3) && str.charAt(1) == str.charAt(2) && str.charAt(0) != str.charAt(1)
  }

  @tailrec
  def containsABBA(str: String): Boolean = {
    if (str.length < 4) false
    else if (isABBA(str.substring(0, 4))) true
    else containsABBA(str.tail)
  }

  def supportsTLS(str: String): Boolean = {
    val (valid, invalid) = separate(str, "", List(), List(), true)
    valid.exists(containsABBA) && !invalid.exists(containsABBA)
  }

  // =====================================================

  def isABA(str: String): Boolean = {
    if (str.length != 3) false
    else str.charAt(0) == str.charAt(2) && str.charAt(0) != str.charAt(1)
  }

  def isBABForABA(str: String, aba: String): Boolean = {
    if (str.length != 3) false
    else str.charAt(0) == aba.charAt(1) && str.charAt(2) == aba.charAt(1) && str.charAt(1) == aba.charAt(0)
  }

  @tailrec
  def containsABA(str: String, acc: List[String]): List[String] = {
    if (str.length < 3) acc
    else if (isABA(str.substring(0, 3))) containsABA(str.tail, acc :+ str.substring(0, 3))
    else containsABA(str.tail, acc)
  }

  def containsBABForABA(str: String, aba: String): Boolean = {
    if (str.length < 3) false
    else if (isBABForABA(str.substring(0, 3), aba)) true
    else containsBABForABA(str.tail, aba)
  }

  def supportsSSL(str: String): Boolean = {
    val (valid, invalid) = separate(str, "", List(), List(), true)
    valid
      .flatMap(v => containsABA(v, List()))
      .exists(aba =>
        invalid.exists(i =>
          containsBABForABA(i, aba)))
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day07-input.txt")).getLines().toList
    val tls = lines.filter(tls)
    tls foreach println
    println(tls.length)

    val ssl = lines.filter(supportsSSL)
    ssl foreach println
    println(ssl.length)
  }
}

package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

object Day07 {

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

    val (valid, invalid) = separate(str, "", List(), List(), true)
    valid.exists(containsABBA) && !invalid.exists(containsABBA)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day07-input.txt")).getLines().toList
    val supported = lines.filter(supportsTLS)
    supported foreach println
    println(supported.length)
  }
}

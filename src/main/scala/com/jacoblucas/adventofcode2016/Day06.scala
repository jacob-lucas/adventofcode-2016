package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

object Day06 {

  @tailrec
  def errorCorrectMessage(messages: List[String], i: Int, msg: String): String = {
    if (i == messages.head.length) msg
    else {
      val ch = messages
        .map(m => (m.charAt(i), messages.count(msg => msg.charAt(i) == m.charAt(i))))
        .distinct
        .sortBy(_._2)
        .head
        ._1

      errorCorrectMessage(messages, i + 1, msg :+ ch)
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day06-input.txt")).getLines().toList
    val msg = errorCorrectMessage(lines, 0, "")
    println(msg)
  }

}

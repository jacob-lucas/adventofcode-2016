package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

object Day06 {

  @tailrec
  def errorCorrectMessage(messages: List[String], i: Int, length: Int, msg: String): String = {
    if (i == length) msg
    else {
      val counts = for {
        m <- messages
      } yield {
        (m.charAt(i), messages.count(msg => msg.charAt(i) == m.charAt(i)))
      }
      val ch = counts
        .distinct
        .sortBy(_._2)
        .head
        ._1

      errorCorrectMessage(messages, i + 1, length, msg :+ ch)
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day06-input.txt")).getLines().toList
    val msg = errorCorrectMessage(lines, 0, lines.head.length, "")
    println(msg)
  }

}

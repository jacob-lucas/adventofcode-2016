package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

object Day02 {

  val keypad = Array(
    Array(1, 2, 3),
    Array(4, 5, 6),
    Array(7, 8, 9)
  )

  def move(from: (Int, Int), dir: Char): (Int, Int) = {
    dir match {
      case 'U' if from._1 > 0                           => (from._1 - 1, from._2)
      case 'D' if from._1 < keypad.length - 1           => (from._1 + 1, from._2)
      case 'L' if from._2 > 0                           => (from._1, from._2 - 1)
      case 'R' if from._2 < keypad(from._1).length - 1  => (from._1, from._2 + 1)
      case _                                            => from
    }
  }

  @tailrec
  def getKeypadNumber(input: String, pos: (Int, Int)): (Int, Int) = {
    input.toList match {
      case x :: xs => getKeypadNumber(xs.mkString, move(pos, x))
      case Nil => pos
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day02-input.txt")).getLines()
    var start = (1, 1)
    for {
      l <- lines
    } {
      start = getKeypadNumber(l, start)
      println(keypad(start._1)(start._2))
    }
  }
}

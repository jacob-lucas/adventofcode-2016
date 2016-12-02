package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

object Day02 {

  val keypad = Array(
    Array(0, 0, 1, 0, 0),
    Array(0, 2, 3, 4, 0),
    Array(5, 6, 7, 8, 9),
    Array(0, "A", "B", "C", 0),
    Array(0, 0, "D", 0, 0)
  )

  def move(from: (Int, Int), dir: Char): (Int, Int) = {
    val to = dir match {
      case 'U' if from._1 > 0                           => (from._1 - 1, from._2)
      case 'D' if from._1 < keypad.length - 1           => (from._1 + 1, from._2)
      case 'L' if from._2 > 0                           => (from._1, from._2 - 1)
      case 'R' if from._2 < keypad(from._1).length - 1  => (from._1, from._2 + 1)
      case _                                            => from
    }

    if (keypad(to._1)(to._2) == 0) from else to
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
    var start = (2, 0)
    for {
      l <- lines
    } {
      start = getKeypadNumber(l, start)
      println(keypad(start._1)(start._2))
    }
  }

}

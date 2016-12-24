package com.jacoblucas.adventofcode2016

import scala.io.Source

case class Disc(id: Int, positions: Int, startPos: Int) {
  def positionAtTime(t: Int): Int = (startPos + t) % positions
}

object Day15 {

  // Disc #1 has 5 positions; at time=0, it is at position 4.
  def parse(str: String): Option[Disc] = {
    try {
      val parts = str.split(" ")
      val id = parts(1).charAt(1).asDigit
      val positions = parts(3).toInt
      val startPos = parts.last.charAt(0).asDigit
      Some(Disc(id, positions, startPos))
    } catch {
      case _: Throwable => None
    }
  }

  def capsuleFallsThrough(discs: List[Disc]): Int = {
    var i = 1
    var fallsThrough = false
    while (!fallsThrough) {
      val res = for {
        j <- discs.indices
      } yield {
        discs(j).positionAtTime(i + j) == 0
      }
      if (res.contains(false)) {
        i = i + 1
      } else {
        fallsThrough = true
      }
    }
    i - 1
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day15-input.txt")).getLines().toList
    val discs = lines flatMap parse sortBy(_.id)

    val t = capsuleFallsThrough(discs)
    println("Capsule falls through at time: " + t)
  }

}

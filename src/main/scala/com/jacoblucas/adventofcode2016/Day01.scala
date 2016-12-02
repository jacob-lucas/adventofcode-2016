package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

sealed trait Direction
case object Left extends Direction
case object Right extends Direction
case class Instruction(direction: Direction, n: Int)

sealed trait Heading
case object North extends Heading
case object South extends Heading
case object East extends Heading
case object West extends Heading

object Day01 {

  def visitedLocations(current: (Int, Int), instruction: Instruction, heading: Heading): (Seq[(Int, Int)], Heading) = {
    if ((heading == North && instruction.direction == Right) || (heading == South && instruction.direction == Left)) {
      // along x-axis +ve
      val locs = for {
        dx <- current._1 + 1 to current._1 + instruction.n
      } yield {
        (dx, current._2)
      }
      (locs, East)
    } else if ((heading == North && instruction.direction == Left) || (heading == South && instruction.direction == Right)) {
      // along x-axis -ve
      val locs = for {
        dx <- (current._1 - instruction.n) until current._1
      } yield {
        (dx, current._2)
      }
      (locs.reverse, West)
    } else if ((heading == West && instruction.direction == Right) || (heading == East && instruction.direction == Left)) {
      // along y-axis +ve
      val locs = for {
        dy <- current._2 + 1 to current._2 + instruction.n
      } yield {
        (current._1, dy)
      }
      (locs, North)
    } else {
      // along y-axis -ve
      val locs = for {
        dy <- (current._2 - instruction.n) until current._2
      } yield {
        (current._1, dy)
      }
      (locs.reverse, South)
    }
  }

  @tailrec
  def walkToEnd(instructions: List[Instruction], locs: List[(Int, Int)], heading: Heading): (Int, Int) = {
    val loc = locs.last
    instructions match {
      case x :: xs =>
        println("Current loc = " + loc + ", facing: " + heading + ", processing: " + x)
        val (newLocs, h) = visitedLocations(loc, x, heading)
        val seenLocs = newLocs.filter(l => locs.count(_ == l) == 1)
        if (seenLocs.nonEmpty) {
          seenLocs.head
        } else {
          walkToEnd(xs, locs ++ newLocs, h)
        }
      case Nil => loc
    }
  }

  def blocks(x: (Int, Int), y: (Int, Int)): Int = {
    math.abs(x._1 - y._1) + math.abs(x._2 - y._2)
  }

  def blocksAway(instructions: List[Instruction]): Int = {
    val start = (0, 0) // could be anywhere
    val end = walkToEnd(instructions, List(start), North)
    blocks(start, end)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day01-input.txt")).mkString
    val instructions = lines
      .split(",")
      .map(_.trim)
      .flatMap(s => {
        try {
          val dirStr = s.substring(0, 1)
          val dir = dirStr match {
            case "L" => Left
            case "R" => Right
            case default =>
              throw new IllegalArgumentException("invalid direction: " + dirStr)
          }
          val n = s.substring(1).toInt
          Some(Instruction(dir, n))
        } catch {
          case e: Exception =>
            println("Ignoring invalid input: " + s)
            None
        }
      })
      .toList

    println(Day01.blocksAway(instructions))
  }

}

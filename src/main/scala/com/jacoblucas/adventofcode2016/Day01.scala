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

  def blocksAway(instructions: List[Instruction]): Int = {

    @tailrec
    def walkToEnd(instructions: List[Instruction], loc: (Int, Int), heading: Heading): (Int, Int) = {
      instructions match {
        case x :: xs =>
//          println("Current loc = " + loc + ", processing: " + x)
          val (dx, dy, h) = heading match {
            case North =>
              x.direction match {
                case Left => (loc._1 - x.n, loc._2, West)
                case Right => (loc._1 + x.n, loc._2, East)
              }
            case South =>
              x.direction match {
                case Left => (loc._1 + x.n, loc._2, East)
                case Right => (loc._1 - x.n, loc._2, West)
              }
            case East =>
              x.direction match {
                case Left => (loc._1, loc._2 + x.n, North)
                case Right => (loc._1, loc._2 - x.n, South)
              }
            case West =>
              x.direction match {
                case Left => (loc._1, loc._2 - x.n, South)
                case Right => (loc._1, loc._2 + x.n, North)
              }
          }
          walkToEnd(xs, (dx, dy), h)
        case Nil => loc
      }
    }

    val start = (0, 0) // could be anywhere
    val end = walkToEnd(instructions, start, North)
    math.abs(start._1 - end._1) + math.abs(start._2 - end._2)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day01-input.txt")).mkString
    val instructions = lines
      .split(",")
      .map(_.trim)
      .map(s => {
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
      .filter(_.nonEmpty)
      .map(_.get)
      .toList

    println(Day01.blocksAway(instructions))
  }

}

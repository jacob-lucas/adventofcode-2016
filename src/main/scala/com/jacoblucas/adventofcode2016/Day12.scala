package com.jacoblucas.adventofcode2016

import scala.io.Source

object Day12 {

  def build(str: String): (String, String, String) = {
    val parts = str.split(" ")
    if (parts.length == 2) (parts(0), parts(1), "")
    else (parts(0), parts(1), parts(2))
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day12-input.txt")).getLines().toArray
    val instructions = lines.map(build)

    var i = 0
    var rs: Map[Char, Int] = Map(
      'a' -> 0,
      'b' -> 0,
      'c' -> 1,
      'd' -> 0
    )

    while (i < instructions.length) {
      val inst = instructions(i)
      println((i+1) + ") Executing: " + inst)

      inst._1 match {
        case "jnz" =>
          val from = inst._2
          val value =
            if (from == "a" || from == "b" || from == "c" || from == "d") {
              rs(from.head)
            } else {
              from.toInt
            }
          if (value != 0) {
            println("jnz by " + inst._3)
            i += inst._3.toInt
          } else {
            i = i + 1
          }

        case "cpy" =>
          val from = inst._2
          val value =
            if (from == "a" || from == "b" || from == "c" || from == "d") {
              rs(from.head)
            } else {
              from.toInt
            }
          rs = rs updated (inst._3.head, value)
          i = i + 1

        case "inc" =>
          val value = rs(inst._2.head)
          rs = rs updated (inst._2.head, value + 1)
          i = i + 1

        case "dec" =>
          val value = rs(inst._2.head)
          rs = rs updated (inst._2.head, value - 1)
          i = i + 1
      }

      println(rs)
    }
  }

}

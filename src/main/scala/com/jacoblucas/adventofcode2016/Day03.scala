package com.jacoblucas.adventofcode2016

import scala.io.Source

object Day03 {

  /**
    * In a valid triangle, the sum of any two sides must be larger than the remaining side.
    * For example, the "triangle" given above is impossible, because 5 + 10 is not larger than 25.
    */
  def isTriangle(tri: Array[Int]): Boolean = {
    if (tri.length != 3) false
    else tri.permutations.forall(t => t(0) + t(1) > t(2))
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day03-input.txt")).getLines()
    val triangles = lines
      .map(_.split(" "))
      .map(_.flatMap(x => {
          try {
            Some(x.toInt)
          } catch {
            case _: Throwable => None
          }
        }))
      .filter(_.length == 3)
      .toList

    println(triangles count isTriangle)
  }

}

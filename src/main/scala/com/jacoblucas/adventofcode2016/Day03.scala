package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
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

  @tailrec
  def toColumnarTriangles(pos: (Int, Int), input: Array[Array[Int]], output:Array[Array[Int]]): Array[Array[Int]] = {
    if (pos == (0, 3)) {
      output
    } else {
      val i = pos._1
      val j = pos._2

      val arr = Array[Int](input(i)(j), input(i+1)(j), input(i+2)(j))

      val nextI = if (i+3 == input.length) 0 else i+3
      val nextJ = if (i+3 == input.length) j+1 else j
      toColumnarTriangles((nextI, nextJ), input, output :+ arr)
    }
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
      .toArray

    val colTriangles = toColumnarTriangles((0, 0), triangles, Array(Array()))

    println(colTriangles count isTriangle)
  }

}

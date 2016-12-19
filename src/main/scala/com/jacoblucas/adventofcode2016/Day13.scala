package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.collection.mutable

object Day13 {

  val input: Int = 1364
  val wid: Int = 45
  val len: Int = 45
  val goal: (Int, Int) = (31, 39)
//  val input = 10
//  val wid = 10
//  val len = 7
//  val to = (7, 4)

  val counts: mutable.MutableList[List[(Int, Int)]] = mutable.MutableList()

  @tailrec
  def binary(n: Int, bin: List[Int] = List()): String = {
    if (n/2 == 1) {
      (1 :: (n % 2) :: bin).mkString
    }
    else {
      val r = n % 2
      val q = n / 2
      binary(q, r :: bin)
    }
  }

  def isWall(x: Int, y: Int): Boolean = {
    val eqn = x*x + 3*x + 2*x*y + y + y*y + input
    binary(eqn).count(_ == '1') % 2 != 0
  }

  def findPath(from: (Int, Int), to: (Int, Int), seen: List[(Int, Int)], coords: List[(Int, Int)]): Unit = {
    if (from == to) {
      counts += coords
    } else {
      List((0, 1), (-1,0), (1,0), (0, -1))
        .map(d => (from._1 + d._1, from._2 + d._2))
        .filter(d => d._1 >= 0 && d._1 < wid && d._2 >= 0 && d._2 < len)
        .filterNot(d => isWall(d._1, d._2))
        .filterNot(d => seen.contains(d))
        .foreach(d => findPath(d, to, seen :+ from, coords :+ d))
    }
  }

  def main(args: Array[String]): Unit = {
    // build plan
    val plan: Array[Array[String]] = Array.fill(len)(Array.fill(wid)("."))
    for {
      x <- 0 until wid
      y <- 0 until len if isWall(x, y)
    } {
      plan(y)(x) = "#"
    }

    // find the paths through
    findPath((1,1), goal, List(), List())

    // print the shortest one
    plan(1)(1) = "S"
    counts
      .sortBy(_.length)
      .head
      .foreach(c => plan(c._2)(c._1) = "o")
    plan(goal._2)(goal._1) = "E"
    plan.foreach(p => println(p.mkString))
    println(counts.sortBy(_.length).head.length)
  }

}

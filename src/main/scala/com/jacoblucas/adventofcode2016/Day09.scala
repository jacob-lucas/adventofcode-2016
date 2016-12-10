package com.jacoblucas.adventofcode2016

import scala.io.Source

case class Marker(numChars: Int, repeat: Int)

object Marker {
  def build(str: String): Option[Marker] = {
    try {
      val parts = str.split("x")
      Some(Marker(parts(0).toInt, parts(1).toInt))
    } catch {
      case _: Throwable => None
    }
  }
}

object Day09 {

  def decompress(str: String): String = {
    def helper(str: List[Char], acc: String): String = {
      str match {
        case '(' :: cs =>
          val markerStr: String = cs.takeWhile(_ != ')').mkString
          val marker = Marker.build(markerStr)
          val tail = cs.takeRight(cs.length - markerStr.length - 1)
          marker match {
            case Some(m) =>
              val d = for {
                i <- 1 to m.repeat
              } yield {
                tail.take(m.numChars).mkString
              }
              helper(tail.takeRight(tail.length - m.numChars), acc + d.mkString)
            case None =>
              helper(tail, acc)
          }
        case c :: cs =>
          helper(cs, acc + c)
        case Nil =>
          acc
      }
    }

    helper(str.toList, "")
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day09-input.txt")).getLines().toList
    val decompressed = lines.map(l => (l,  decompress(l)))
//    decompressed.foreach(d => println(d._1 + " => " + d._2))
    println(decompressed.map(_._2.length).sum)
  }
}

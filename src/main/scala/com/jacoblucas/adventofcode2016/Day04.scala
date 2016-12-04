package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/**
  * Each room consists of an encrypted name (lowercase letters separated by dashes)
  * followed by a dash, a sector ID, and a checksum in square brackets.
  */
class Room(val name: String, val sectorID: Int, val checksum: String) {

  /**
    * A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name,
    * in order, with ties broken by alphabetization.
    */
  def isReal: Boolean = {
    case class CharCount(c: Char, n: Int) extends Ordered[CharCount] {
      override def compare(that: CharCount): Int = {
        if (n < that.n || (n == that.n && c > that.c)) -1
        else if (n == that.n && c == that.c) 0
        else 1
      }
    }

    @tailrec
    def check(checksum: String, counts: mutable.PriorityQueue[CharCount], checkCount: Int): Boolean = {
      if (counts.isEmpty || checkCount == 5) {
        true
      } else {
        val head = counts.dequeue()
        checksum.toList match {
          case c :: cs if c == head.c => check(cs.mkString, counts, checkCount+1)
          case _ => false
        }
      }
    }

    val maxHeap = mutable.PriorityQueue[CharCount]()
    name
      .replaceAll("-", "")
      .map(c => CharCount(c, name.count(_ == c)))
      .distinct
      .foreach(maxHeap += _)

    check(checksum, maxHeap, 0)
  }

  def decrypt: String = {
    def rotate(c: Char): Char = {
      c match {
        case 'z' => 'a'
        case _ => (c+1).toChar
      }
    }

    @tailrec
    def rotateN(c: Char, n: Int): Char = {
      if (c == '-') ' '
      else if (n == 0) c
      else rotateN(rotate(c), n-1)
    }

    name
      .toCharArray
      .map(c => rotateN(c, sectorID))
      .mkString
  }

  override def toString: String = {
    name + "-" + sectorID + "[" + checksum + "]"
  }

}

object Room {

  def build(string: String): Option[Room] = {
    // e.g. aaaaa-bbb-z-y-x-123[abxyz]
    // name = aaaaa-bbb-z-y-x
    // sector ID = 123
    // checksum = abxyz
    try {
      val parts = string.split("-")
      val len = parts.length
      val idcs = parts.takeRight(1)(0).split("\\[")
      val name = parts.take(len-1).mkString("-")
      val sectorId = idcs(0).toInt
      val checksum = idcs(1).take(idcs(1).length-1)
      Some(new Room(name, sectorId, checksum))
    } catch {
      case _: Throwable => None
    }
  }

}

object Day04 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day04-input.txt")).getLines()
    val rooms = lines flatMap Room.build filter (_.isReal)
    rooms.foreach(r => {
      val decrypted = r.decrypt
      println(r + " = " + decrypted)
    })
  }

}

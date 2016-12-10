package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.io.Source

sealed trait Command
sealed case class Rect(x: Int, y: Int) extends Command // x wide by y tall rectangle
sealed case class RotateRow(n: Int, amount: Int) extends Command // rotate row n by amount
sealed case class RotateCol(n: Int, amount: Int) extends Command // rotate col n by amount

class Screen(val arr: Array[Array[String]]) {

  def on(x: Int, y: Int)      = arr(y)(x) = Screen.on
  def off(x: Int, y: Int)     = arr(y)(x) = Screen.off
  def toggle(x: Int, y: Int)  = if (arr(y)(x) == Screen.off) on(x, y) else off(x, y)
  def count(state: String)    = arr.map(_.count(_ == state)).sum

  def execute(cmd: Command) = {
    println("Executing " + cmd)
    cmd match {
      case r:Rect =>
        for {
          x <- 0 until r.x
          y <- 0 until r.y
        } {
          on(x, y)
        }

      case row:RotateRow =>
        val orig = arr(row.n)
        var i = 0
        val x = arr(row.n).map(ch => {
          val j = if (i - row.amount < 0) i + arr(row.n).length - row.amount else i - row.amount
          i = i + 1
          orig(j)
        })
        arr(row.n) = x

      case col:RotateCol =>
        val orig = arr.map(_.clone())
        var j = 0
        arr.foreach(s => {
          val i = if (j - col.amount < 0) j + arr.length - col.amount else j - col.amount
          arr(j)(col.n) = orig(i)(col.n)
          j = j + 1
        })
    }
    println(this)
  }

  override def toString: String = {
    @tailrec
    def helper(arr: Array[Array[String]], acc: String): String = {
      if (arr.length == 0) acc
      else {
        helper(arr.tail, acc + arr.head.mkString + "\n")
      }
    }
    helper(arr, "")
  }

}

object Screen {
  val off = "."
  val on = "#"

  // create an x wide by y tall screen
  def init(x: Int, y: Int): Screen = {
    val arr = Array.fill(y)(Array.fill(x)(off))
    new Screen(arr)
  }
}

object Day08 {

  def toCommand(str: String): Option[Command] = {
    try {
      val parts = str.split(" ")
      if (parts(0) == "rect") {
        val rectParts: Array[String] = parts(1).split("x")
        Some(Rect(rectParts(0).toInt, rectParts(1).toInt))
      } else {
        val i = parts(2).split("=")(1).toInt
        if (parts(1) == "row") {
          Some(RotateRow(i, parts.last.toInt))
        } else {
          Some(RotateCol(i, parts.last.toInt))
        }
      }
    } catch {
      case _: Throwable => None
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day08-input.txt")).getLines().toList
    val commands = lines flatMap toCommand
    val screen = Screen.init(50, 6)
    commands foreach screen.execute
    println(screen.count(Screen.on))
  }

}

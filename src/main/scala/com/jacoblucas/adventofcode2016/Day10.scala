package com.jacoblucas.adventofcode2016

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

abstract class ChipList(val chips: mutable.MutableList[Int]) {
  def accept(value: Int): ChipList = {
    chips += value
    this
  }
}
class Bot(val id: Int, override val chips: mutable.MutableList[Int], val instruction: BotInstruction) extends ChipList(chips: mutable.MutableList[Int]) {
  override def accept(value: Int): Bot = {
    super.accept(value)
    if (chips.length == 2) {
      Day10.execute(instruction)
    }
    this
  }

  override def toString: String = {
    "[" + id + "] : (" + chips.mkString(",") + ")"
  }
}

class Output(val id: Int, override val chips: mutable.MutableList[Int]) extends ChipList(chips: mutable.MutableList[Int]) {
  override def toString: String = {
    "[" + id + "] : (" + chips.mkString(",") + ")"
  }
}

case class Assignment(id: Int, value: Int)
object Assignment {
  def build(str: String): Option[Assignment] = {
    try {
      val parts = str.split(" ")
      Some(Assignment(parts(5).toInt, parts(1).toInt))
    } catch {
      case _: Throwable => None
    }
  }
}

case class BotInstruction(fromId: Int, lowId: Int, lowType: String, highId: Int, highType: String)
object BotInstruction {
  def build(str: String): Option[BotInstruction] = {
    try {
      val parts = str.split(" ")
      Some(BotInstruction(parts(1).toInt, parts(6).toInt, parts(5), parts(11).toInt, parts(10)))
    } catch {
      case _: Throwable => None
    }
  }
}

object Day10 {

  var botMap: Map[Int, Bot] = Map()
  var outputMap: Map[Int, Output] = Map()

  def execute(instruction: BotInstruction) = {
    try {
      if (botMap.contains(instruction.fromId)) {
        val fromBot = botMap(instruction.fromId)
        val lowDest = if (instruction.lowType == "bot") botMap(instruction.lowId) else outputMap(instruction.lowId)
        val highDest = if (instruction.highType == "bot") botMap(instruction.highId) else outputMap(instruction.highId)
        val low = fromBot.chips.min
        val high = fromBot.chips.max
        lowDest.accept(low)
        highDest.accept(fromBot.chips.max)
        fromBot.chips.clear()
        println(
          "bot " + fromBot.id + " gives " + low + " to " + instruction.lowType + " " + instruction.lowId +
            " and " + high + " to " + instruction.highType + " " + instruction.highId)
      }
    }
    catch {
      case e: Exception =>
        println("Unable to process instruction: " + instruction + " due to " + e.getMessage)
    }
  }

  @tailrec
  def assign(assignments: List[Assignment], botMap: Map[Int, Bot]): Map[Int, Bot] = {
    assignments match {
      case a :: as =>
        if (botMap.contains(a.id)) {
          val bot = botMap(a.id).accept(a.value)
          assign(as, botMap updated (a.id, bot))
        } else {
          println("WARN: no bot exists for " + a)
          assign(as, botMap)
        }
      case Nil => botMap
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day10-input.txt")).getLines().toList
    val assignments = lines filter (_ contains "value") flatMap Assignment.build
    val instructions = lines filter (_ contains "gives") flatMap BotInstruction.build

    outputMap = outputMap ++ instructions
      .filter(i => i.lowType == "output")
      .map(i => (i.lowId, new Output(i.lowId, mutable.MutableList())))
      .toMap

    outputMap = outputMap ++ instructions
      .filter(i => i.highType == "output")
      .map(i => (i.highId, new Output(i.lowId, mutable.MutableList())))
      .toMap

    botMap = instructions
      .map(i => i.fromId -> new Bot(i.fromId, mutable.MutableList(), i))
      .toMap

    assign(assignments, botMap)

    println(outputMap(0).chips.head * outputMap(1).chips.head * outputMap(2).chips.head)
  }

}

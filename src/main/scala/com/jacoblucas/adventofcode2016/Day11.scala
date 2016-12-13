package com.jacoblucas.adventofcode2016

import scala.collection.mutable

sealed trait Material
case class Microchip(name: String) extends Material {
  override def toString: String = name.head.toUpper + "M"
}
case class Generator(chipCompatibility: Microchip) extends Material {
  override def toString: String = chipCompatibility.name.head.toUpper + "G"
}

class Elevator(l: Int, val minCapacity: Int, val maxCapacity: Int) {
  var level = l
  def setLevel(l: Int) = level = l
}

case class Floor(level: Int, materials: mutable.MutableList[Material]) {
  def add(m: Material) = materials += m

  def remove(m: Material) = {
    val tmp = materials.clone()
    materials.clear()
    tmp.filterNot(_ == m).foreach(m => materials += m)
  }

  def toString(hasElevator: Boolean): String = "|F" + level + " " + (if (hasElevator) "E " else "  ") + "| " + materials.sortBy(_.toString).mkString(" ")
}

case class Move(from: Floor, to:Floor, materials: List[Material]) {
  def safe: Boolean = {
    val remaining = from.materials.filterNot(m => materials.contains(m))
    val adjusted = to.materials ++ materials

    def safe(ms: List[Material]): Boolean = {
      if (ms.length <= 1) true
      else if (!ms.exists(_.isInstanceOf[Generator])) true
      else if (ms.filter(_.isInstanceOf[Microchip]).forall(m => {
        ms
          .filter(_.isInstanceOf[Generator])
          .map(_.asInstanceOf[Generator])
          .map(_.chipCompatibility)
          .contains(m)
      })) true
      else false
    }

    safe(remaining.toList) && safe(adjusted.toList)
  }

  override def toString: String = "Moved " + materials.mkString(",") + " from L" + from.level + " to L" + to.level
}

case class Facility(floors: mutable.MutableList[Floor], elevator: Elevator) {
  var visited = false
  def setVisited(b: Boolean) = {
    visited = b
  }

  def score: Int = floors.map(f => f.level * f.materials.length).sum

  def move(m: Move) = {
    if (!(floors.map(_.level).contains(m.from.level) && floors.map(_.level).contains(m.to.level))) {
      println("WARN: facility does not contain floor(s): " + m.from.level + ", " + m.to.level)
    } else if (!m.materials.forall(m.from.materials.contains(_))) {
      println("WARN: Floor " + m.from + " does not contain all requested materials: " + m.materials)
    } else if (elevator.level != m.from.level) {
      println("WARN: Elevator not at level: " + m.from.level)
    } else if (m.materials.length < elevator.minCapacity || m.materials.length > elevator.maxCapacity) {
      println("WARN: Elevator capacity constraints not met to move " + m.materials + " from L" + m.from.level + " to L" + m.to.level)
    } else if (!m.safe) {
      println("WARN: Unsafe to move " + m.materials + " from L" + m.from.level + " to L" + m.to.level)
    } else {
      m.materials.foreach(mo => {
        m.from.remove(mo)
        m.to.add(mo)
        elevator.setLevel(m.to.level)
      })
      println(m)
      println(this)
    }
  }

  override def toString: String = {
    floors
      .reverse
      .map(f => f.toString(elevator.level == f.level))
      .mkString("\n")
  }
}

object Day11 {
  def main(args: Array[String]): Unit = {
//    val strontiumChip = Microchip("strontium")
//    val plutoniumChip = Microchip("plutonium")
//    val thuliumChip = Microchip("thulium")
//    val rutheniumChip = Microchip("ruthenium")
//    val curiumChip = Microchip("curium")
//    val strontiumGen = Generator(strontiumChip)
//    val plutoniumGen = Generator(plutoniumChip)
//    val rutheniumGen = Generator(rutheniumChip)
//    val thuliumGen = Generator(thuliumChip)
//    val curiumGen = Generator(curiumChip)

    val hydrogen = Microchip("hydrogen")
    val lithium = Microchip("lithium")
    val hydrogenGen = Generator(hydrogen)
    val lithiumGen = Generator(lithium)

//    val f1: Floor = Floor(1, mutable.MutableList(strontiumChip, plutoniumChip, strontiumGen, plutoniumGen))
//    val f2: Floor = Floor(2, mutable.MutableList(rutheniumChip, curiumChip, thuliumGen, rutheniumGen, curiumGen))
//    val f3: Floor = Floor(3, mutable.MutableList(thuliumChip))
    val f1: Floor = Floor(1, mutable.MutableList(hydrogen, lithium))
    val f2: Floor = Floor(2, mutable.MutableList(hydrogenGen))
    val f3: Floor = Floor(3, mutable.MutableList(lithiumGen))
    val f4: Floor = Floor(4, mutable.MutableList())
    val facility = Facility(mutable.MutableList(f1, f2, f3, f4), new Elevator(1, 1, 2))

    facility.move(Move(f1, f2, List(hydrogen)))
    facility.move(Move(f2, f3, List(hydrogen, hydrogenGen)))
    facility.move(Move(f3, f2, List(hydrogen)))
    facility.move(Move(f2, f1, List(hydrogen)))
    facility.move(Move(f1, f2, List(hydrogen, lithium)))
    facility.move(Move(f2, f3, List(hydrogen, lithium)))
    facility.move(Move(f3, f4, List(hydrogen, lithium)))
    facility.move(Move(f4, f3, List(hydrogen)))
    facility.move(Move(f3, f4, List(hydrogenGen, lithiumGen)))
    facility.move(Move(f4, f3, List(lithium)))
    facility.move(Move(f3, f4, List(hydrogen, lithium)))

    val queue = new mutable.Queue[Facility]()
    queue.enqueue(facility)
    while (queue.nonEmpty) {
      val f = queue.dequeue()
      if (!f.visited) {
        if (f.score == f.floors.length * f.floors.map(_.materials.length).sum) {
          // win
          println("WIN!")
          queue.clear()
        } else {
          // create new facilities based on f
          // add to queue
          f.setVisited(true)
          println(f)
        }
      }
    }
  }
}

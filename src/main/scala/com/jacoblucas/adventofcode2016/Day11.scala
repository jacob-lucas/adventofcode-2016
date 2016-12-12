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

case class Facility(floors: mutable.MutableList[Floor], elevator: Elevator) {
  def safeMove(from: Floor, to: Floor, materials: List[Material]): Boolean = {
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

  def move(from: Floor, to:Floor, materials: List[Material]) = {
    if (!(floors.map(_.level).contains(from.level) && floors.map(_.level).contains(to.level))) {
      println("WARN: facility does not contain floor(s): " + from.level + ", " + to.level)
    } else if (!materials.forall(from.materials.contains(_))) {
      println("WARN: Floor " + from + " does not contain all requested materials: " + materials)
    } else if (elevator.level != from.level) {
      println("WARN: Elevator not at level: " + from.level)
    } else if (materials.length < elevator.minCapacity || materials.length > elevator.maxCapacity) {
      println("WARN: Elevator capacity constraints not met to move " + materials + " from L" + from.level + " to L" + to.level)
    } else if (!safeMove(from, to, materials)) {
      println("WARN: Unsafe to move " + materials + " from L" + from.level + " to L" + to.level)
    } else {
      materials.foreach(m => {
        from.remove(m)
        to.add(m)
        elevator.setLevel(to.level)
        println("Moved " + m + " from L" + from.level + " to L" + to.level)
      })
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

    println(facility)
    facility.move(f1, f2, List(hydrogen))
    facility.move(f2, f3, List(hydrogen, hydrogenGen))
    facility.move(f3, f2, List(hydrogen))
    facility.move(f2, f1, List(hydrogen))
    facility.move(f1, f2, List(hydrogen, lithium))
    facility.move(f2, f3, List(hydrogen, lithium))
    facility.move(f3, f4, List(hydrogen, lithium))
    facility.move(f4, f3, List(hydrogen))
    facility.move(f3, f4, List(hydrogenGen, lithiumGen))
    facility.move(f4, f3, List(lithium))
    facility.move(f3, f4, List(hydrogen, lithium))
  }
}

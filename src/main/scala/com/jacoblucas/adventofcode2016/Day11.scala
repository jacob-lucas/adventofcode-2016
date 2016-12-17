package com.jacoblucas.adventofcode2016

import scala.collection.mutable

sealed trait Material
case class Microchip(name: String) extends Material {
  override def toString: String = name.head.toUpper + "M"
}
case class Generator(chipCompatibility: Microchip) extends Material {
  override def toString: String = chipCompatibility.name.head.toUpper + "G"
}

case class Elevator(level: Int, minCapacity: Int, maxCapacity: Int)

case class Floor(level: Int, materials: List[Material]) {
  def add(ms: List[Material]): Floor          = Floor(level, materials ++ ms)
  def remove(ms: List[Material]): Floor       = Floor(level, materials.filterNot(ms.contains(_)))
  def toString(hasElevator: Boolean): String  = "|F" + level + " " + (if (hasElevator) "E " else "  ") + "| " + materials.sortBy(_.toString).mkString(" ")
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

    safe(remaining) && safe(adjusted)
  }

  override def toString: String = "Moved " + materials.mkString(",") + " from L" + from.level + " to L" + to.level
}

case class Facility(floors: List[Floor], elevator: Elevator) {

  def possibleMoves: List[Move] = {
    val currFloor = floors(elevator.level - 1)
    val aboveFloor = if (elevator.level == floors.length) None else Some(floors(elevator.level))
    val belowFloor = if (elevator.level == 1) None else Some(floors(elevator.level - 2))

    def getMoves(floor: Option[Floor]): List[Move] = {
      floor match {
        case Some(f) if currFloor.materials.nonEmpty => // currFloor.materials.permutations.toList.map(ms => Move(currFloor, f, ms))
          val ms = for {
            n <- elevator.minCapacity to elevator.maxCapacity
          } yield {
            currFloor
              .materials
              .permutations
              .map(ms => Move(currFloor, f, ms.take(n)))
              .toList
          }
          ms.flatten.toList
        case _ => List()
      }
    }

    (getMoves(aboveFloor) ++ getMoves(belowFloor)).sortWith(move(_).score > move(_).score)
  }

  def score: Int = floors.map(f => f.level * f.materials.length).sum

  def move(m: Move): Facility = {
    if (!(floors.map(_.level).contains(m.from.level) && floors.map(_.level).contains(m.to.level))) {
//      println("WARN: facility does not contain floor(s): " + m.from.level + ", " + m.to.level)
      this
    } else if (!m.materials.forall(m.from.materials.contains(_))) {
//      println("WARN: Floor " + m.from + " does not contain all requested materials: " + m.materials)
      this
    } else if (elevator.level != m.from.level) {
//      println("WARN: Elevator not at level: " + m.from.level)
      this
    } else if (m.materials.length < elevator.minCapacity || m.materials.length > elevator.maxCapacity) {
//      println("WARN: Elevator capacity constraints not met to move " + m.materials + " from L" + m.from.level + " to L" + m.to.level)
      this
    } else if (!m.safe) {
//      println("WARN: Unsafe to move " + m.materials + " from L" + m.from.level + " to L" + m.to.level)
      this
    } else {
      Facility(
        floors.map(f => {
          f.level match {
            case m.from.level => f.remove(m.materials)
            case m.to.level => f.add(m.materials)
            case _ => f
          }
        }),
        Elevator(m.to.level, elevator.minCapacity, elevator.maxCapacity)
      )
    }
  }

  override def toString: String = {
    floors
      .reverse
      .map(f => f.toString(elevator.level == f.level))
      .mkString("\n") + "\n------------------"
  }

}

object Day11 {
  def main(args: Array[String]): Unit = {
    val strontiumChip = Microchip("strontium")
    val plutoniumChip = Microchip("plutonium")
    val thuliumChip = Microchip("thulium")
    val rutheniumChip = Microchip("ruthenium")
    val curiumChip = Microchip("curium")
    val strontiumGen = Generator(strontiumChip)
    val plutoniumGen = Generator(plutoniumChip)
    val rutheniumGen = Generator(rutheniumChip)
    val thuliumGen = Generator(thuliumChip)
    val curiumGen = Generator(curiumChip)

    val hydrogen = Microchip("hydrogen")
    val lithium = Microchip("lithium")
    val hydrogenGen = Generator(hydrogen)
    val lithiumGen = Generator(lithium)

//    val f1: Floor = Floor(1, List(strontiumChip, plutoniumChip, strontiumGen, plutoniumGen).sortBy(_.toString))
//    val f2: Floor = Floor(2, List(rutheniumChip, curiumChip, thuliumGen, rutheniumGen, curiumGen).sortBy(_.toString))
//    val f3: Floor = Floor(3, List(thuliumChip))
    val f1: Floor = Floor(1, List(hydrogen, lithium))//.sortBy(_.toString))
    val f2: Floor = Floor(2, List(hydrogenGen))
    val f3: Floor = Floor(3, List(lithiumGen))
    val f4: Floor = Floor(4, List())

    val visited = mutable.Set[Facility]()
    val queue = new mutable.Queue[(Facility, List[Move])]()
    queue.enqueue((Facility(List(f1, f2, f3, f4), Elevator(1, 1, 2)), List()))
    while (queue.nonEmpty) {
      val (f, ms) = queue.dequeue()
      if (f.score == f.floors.length * f.floors.map(_.materials.length).sum) {
        println("WIN!")
        queue.clear()
      } else {
        if (!visited.contains(f)) {
          // create new facilities based on f
          // add to queue
          visited.add(f)
          val pms = f.possibleMoves.filter(_.safe)

          pms.foreach(pm => {
            val fac = f.move(pm)
            queue.enqueue((fac, ms :+ pm))
          })
        }
      }

      println(f)
      println(ms.length)
    }
  }
}

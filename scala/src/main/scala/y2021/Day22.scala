package io.vltmn.aoc
package y2021

import y2021.Day22.{Cuboid, InclusionExclusionCuboid, Instruction, PosRange}

import scala.annotation.tailrec

class Day22 extends Solution {
  def parseInput(input: String): Seq[Instruction] = {
    input.linesIterator
      .map(line => {
        val spaceSplit = line.split(" ")
        val on = spaceSplit.head == "on"
        val ranges = spaceSplit.last.split(",")
          .map(s => s.split("=").last)
          .map(s => s.split("\\.\\.").map(_.toInt))
          .map(r => PosRange(r(0), r(1)))
        Instruction(on, Cuboid(ranges.head, ranges(1), ranges(2)))
      })
      .toSeq
  }

  @tailrec
  final def applyInstructions(instructions: Seq[Instruction], cuboids: Seq[InclusionExclusionCuboid] = Seq()): Seq[InclusionExclusionCuboid] = {
    val instruction :: rest = instructions
    val intersectionsToAdd = cuboids
      .map(e => (e._1, e._2 intersection instruction.cuboid))
      .filter(_._2.isDefined)
      .map(e => (!e._1, e._2.get))
    val nextCuboids = cuboids
      .appendedAll(intersectionsToAdd)
      .appendedAll(if (instruction.on) List((true, instruction.cuboid)) else List())
    if (rest.isEmpty)
      nextCuboids
    else
      applyInstructions(rest, nextCuboids)
  }

  def part1(input: String): Long = {
    val cube = PosRange(-50, 50)
    val instructions = parseInput(input)
      .filter(i => i.cuboid.isInside(cube, cube, cube))
    val done = applyInstructions(instructions)
    done.countCubes
  }

  def part2(input: String): Long = {
    val instructions = parseInput(input)
    val done = applyInstructions(instructions)
    done.countCubes
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

object Day22 {
  type InclusionExclusionCuboid = (Boolean, Cuboid)

  implicit class RichCuboidSeq(x: Seq[InclusionExclusionCuboid]) {
    def countCubes: Long = x.foldLeft(0L)((acc, curr) => if (curr._1) acc + curr._2.volume else acc - curr._2.volume)
  }

  case class PosRange(val start: Int, val end: Int) {
    def isInside(other: PosRange): Boolean = start >= other.start && end <= other.end
  }

  case class Instruction(on: Boolean, cuboid: Cuboid)

  case class Point(x: Int, y: Int, z: Int)

  case class Cuboid(x: PosRange, y: PosRange, z: PosRange) {
    val volume: Long = (x.end - x.start + 1) * (y.end - y.start + 1) * (z.end - z.start.toLong + 1)

    def isInside(x: PosRange, y: PosRange, z: PosRange): Boolean =
      this.x.isInside(x) && this.y.isInside(y) && this.z.isInside(z)

    def isInside(point: Point): Boolean =
      this.isInside(PosRange(point.x, point.x), PosRange(point.y, point.y), PosRange(point.z, point.z))

    def intersection(other: Cuboid): Option[Cuboid] = {
      val minX = if (x.start > other.x.start) x.start else other.x.start
      val maxX = if (x.end < other.x.end) x.end else other.x.end
      val minY = if (y.start > other.y.start) y.start else other.y.start
      val maxY = if (y.end < other.y.end) y.end else other.y.end
      val minZ = if (z.start > other.z.start) z.start else other.z.start
      val maxZ = if (z.end < other.z.end) z.end else other.z.end
      if (minX <= maxX && minY <= maxY && minZ <= maxZ)
        Some(Cuboid(PosRange(minX, maxX), PosRange(minY, maxY), PosRange(minZ, maxZ)))
      else
        None
    }
  }
}

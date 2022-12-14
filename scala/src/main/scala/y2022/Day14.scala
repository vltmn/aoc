package io.vltmn.aoc
package y2022

import util.MapUtils.{Coord, CoordUtils}

import scala.annotation.tailrec

class Day14 extends Solution {
  def moveSand(current: Coord, occupied: Set[Coord], xMin: Int, xMax: Int, yMax: Int): Coord = {
    @tailrec
    def inner(current: Coord): Coord = {
      val (x, y) = current
      if (x < xMin || x > xMax || y >= yMax - 1)
        current
      else if (!occupied.contains((x, y + 1)))
        inner((x, y + 1))
      else if (!occupied.contains((x - 1, y + 1)))
        inner((x - 1, y + 1))
      else if (!occupied.contains((x + 1, y + 1)))
        inner((x + 1, y + 1))
      else
        current
    }

    inner(current)
  }

  def dropSand(occupied: Set[Coord], xMin: Int, xMax: Int, yMax: Int, dropFrom: Coord): Int = {
    @tailrec
    def inner(occupied: Set[Coord], i: Int = 0): Int = {
      val newSandPos = moveSand(dropFrom, occupied, xMin, xMax, yMax)
      if (newSandPos._1 < xMin || newSandPos._1 > xMax || newSandPos._2 > yMax || occupied.contains(newSandPos)) {
        i
      } else
        inner(occupied.incl(newSandPos), i + 1)
    }

    inner(occupied)
  }

  override def solve(input: String): String = {
    val rockCoords: Set[Coord] = input
      .linesIterator
      .flatMap(l => {
        val points: Seq[Coord] = l
          .split(" -> ")
          .map(_.split(",").map(_.toInt))
          .map(e => (e.head, e.last))
        points.tail.foldLeft(points.head, Set[Coord]())((acc, curr) =>
          (curr, acc._2.union(acc._1.drawLine(curr)))
        )._2
      })
      .toSet

    val dropFrom = (500, 0)
    val xMin = rockCoords.map(_._1).min
    val xMax = rockCoords.map(_._1).max
    val yMax = rockCoords.map(_._2).max + 2
    val p1 = dropSand(rockCoords, xMin, xMax, yMax, dropFrom)
    val p2 = dropSand(rockCoords, Int.MinValue, Int.MaxValue, yMax, dropFrom)
    s"Part1: $p1\nPart2: $p2"
  }
}

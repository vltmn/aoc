package io.vltmn.aoc2021
package example

import scala.annotation.tailrec

class Day17 extends Solution {
  case class Area(x0: Int, x1: Int, y0: Int, y1: Int) {
    def pointInside(point: Point): Boolean = point.x >= x0 && point.x <= x1 && point.y >= y0 && point.y <= y1
  }

  case class Vector(x: Int, y: Int) {
    def next(): Vector = Vector(if (x > 0) x - 1 else if (x < 0) x + 1 else 0, y - 1)
  }

  case class Point(x: Int, y: Int) {
    def applyVelocity(velocity: Vector): Point = Point(x + velocity.x, y + velocity.y)
  }

  def parseInput(input: String): Area = {
    val data = input.split(",")
      .map(_.split("=").last)
      .flatMap(_.split("\\.\\."))
      .filter(_.nonEmpty)
      .map(_.toInt)
    Area(data.head, data(1), data(2), data(3))

  }

  def testTrajectory(startPoint: Point, startVelocity: Vector, targetArea: Area): (Int, Point, Boolean) = {
    @tailrec
    def inner(current: Point, velocity: Vector, time: Int): (Int, Point) = {
      val next = current.applyVelocity(velocity)
      val nextVelocity = velocity.next()
      val xDone = next.x > targetArea.x1 || (next.x < targetArea.x0 && nextVelocity.x == 0)
      val yDone = next.y < targetArea.y0 && nextVelocity.y <= 0
      if (targetArea.pointInside(next))
        (time, next)
      else if (xDone || yDone)
        (time, next)
      else
        inner(next, nextVelocity, time + 1)
    }

    val (hitTime, hitPoint) = inner(startPoint, startVelocity, 0)
    (hitTime, hitPoint, targetArea.pointInside(hitPoint))
  }

  def heightReached(startPoint: Point, velocityVector: Vector): (Int, Int) = {
    @tailrec
    def inner(current: Point, velocity: Vector, time: Int): (Int, Int) = {
      val next = current.applyVelocity(velocity)
      val nextVelocity = velocity.next()
      if (nextVelocity.y < 0)
        (time, next.y)
      else
        inner(next, nextVelocity, time + 1)
    }

    inner(startPoint, velocityVector, 0)
  }

  def part1(input: String): Int = {
    val targetArea = parseInput(input)
    val startPoint = Point(0, 0)
    (0 to 400).flatMap(x => (0 to 400).map(y => Vector(x, y)))
      .filter(velocity => testTrajectory(startPoint, velocity, targetArea)._3)
      .map(v => heightReached(startPoint, v)._2)
      .max
  }

  def part2(input: String): Int = {
    val targetArea = parseInput(input)
    val startPoint = Point(0, 0)
    val delta = 400
    (-delta to delta).flatMap(x => (-delta to delta).map(y => Vector(x, y)))
      .count(velocity => testTrajectory(startPoint, velocity, targetArea)._3)

  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

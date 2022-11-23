package io.vltmn.aoc
package y2021

import breeze.linalg._
import breeze.numerics._

import scala.annotation.tailrec

class Day19 extends Solution {
  type Report = Seq[Seq[Point]]

  case class Point(x: Int, y: Int, z: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)

    def -(other: Point): Point = this.+(Point(-other.x, -other.y, -other.z))
  }

  def parseInput(input: String): Report = {
    input.split("\n\n")
      .map(scan => scan.linesIterator.drop(1)
        .map(l => {
          val splitted = l.split(",")
          Point(splitted.head.toInt, splitted(1).toInt, splitted(2).toInt)
        })
        .toSeq
      )
  }

  def generateTransformationMatrices(): Seq[Matrix[Int]] = {
    val angles = Seq(0, Math.PI / 2, Math.PI, Math.PI * 1.5)

    def cos(angle: Double): Int = Math.cos(angle).toInt

    def sin(angle: Double): Int = Math.sin(angle).toInt

    val xs = angles.map(angle => DenseMatrix((1, 0, 0), (0, cos(angle), -sin(angle)), (0, sin(angle), cos(angle))))
    val ys = angles.map(angle => DenseMatrix((cos(angle), 0, sin(angle)), (0, 1, 0), (-sin(angle), 0, cos(angle))))
    val zs = angles.map(angle => DenseMatrix((cos(angle), -sin(angle), 0), (sin(angle), cos(angle), 0), (0, 0, 1)))
    xs.flatMap(x => ys.flatMap(y => zs.map(z => x * y * z))).distinct
  }

  def findBeacons(scanners: Seq[Seq[Point]], minQty: Int): (Seq[Point], Seq[Point]) = {
    val transformationMatrices = generateTransformationMatrices()

    @tailrec
    def inner(done: Set[Point], left: Seq[Seq[Point]], scanners: Seq[Point]): (Seq[Point], Seq[Point]) = {
      val next = left.head
      val rotatedWithDeltas = transformationMatrices
        .map(tm => next
          .map(n => tm * DenseVector(n.x, n.y, n.z))
          .map(v => Point(v(0), v(1), v(2)))
        )
        .flatMap(ps => {
          val pairs = done.flatMap(d => ps.map(p => (d, p)))
          pairs
            // e is a pair of done and rotated point
            // d - p
            .map(e => e._1 - e._2)
            .map(delta => (ps, delta))
        })

      val matchCount = rotatedWithDeltas.map(d => (d._2, d._1.map(_ + d._2)))
        .map(ps => (ps, ps._2.count(p => done.contains(p))))

      val found = matchCount
        .find(e => e._2 >= minQty)
        .map(_._1)
      //.find(ps => ps.count(p => done.contains(p)) > minQty)

      // if found and no left, exit
      // else add found to done and remove from left, recurse

      if (found.nonEmpty && left.tail.isEmpty) {
        val (scannerLoc, foundBeacons) = found.get
        ((done ++ foundBeacons).toSeq, scanners.appended(scannerLoc))
      }
      else {
        val nextLeft = if (found.isEmpty) left.tail ++ List(next) else left.tail
        val nextDone = if (found.isEmpty) done else (done ++ found.get._2)
        val nextScanners = if (found.isEmpty) scanners else scanners.appended(found.get._1)
        inner(nextDone, nextLeft, nextScanners)
      }

    }

    inner(Set() ++ scanners.head, scanners.tail, Seq(Point(0, 0, 0)))
  }

  def manhattanDistance(a: Point, b: Point): Int = Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z)


  def part1(input: String): Int = {
    val scanners = parseInput(input)
    val (beacons, _) = findBeacons(scanners, 12)

    beacons.size
  }

  def part2(input: String): Int = {
    val scanners = parseInput(input)
    val (_, scannersFound) = findBeacons(scanners, 12)
    scannersFound.flatMap(a => scannersFound.map(b => (a, b)))
      .map(p => manhattanDistance(p._1, p._2))
      .max
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

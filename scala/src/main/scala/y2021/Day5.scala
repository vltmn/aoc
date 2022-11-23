package io.vltmn.aoc
package y2021

class Day5 extends Solution {
  def parseInput(input: String): Seq[Line] = {
    input
      .linesIterator
      .map(l => l.split("->").map(_.trim).map(_.split(",").map(_.toInt)))
      .map(d => Line(d(0)(0), d(0)(1), d(1)(0), d(1)(1)))
      .toSeq
  }

  def isDiagonal(l: Line): Boolean = l.x1 != l.x2 && l.y1 != l.y2

  def lineCovers(l: Line): Seq[Point] = {
    val dy = l.y2 - l.y1
    val dx = l.x2 - l.x1
    val diag = () => (0 to dx.abs)
      .map(i => (if (dx > 0) i else -i, if (dy > 0) i else -i))
      .map(ds => Point(l.x1 + ds._1, l.y1 + ds._2))
    val straight = () => {
      val x1 = l.x1 min l.x2
      val x2 = l.x1 max l.x2
      val y1 = l.y1 min l.y2
      val y2 = l.y1 max l.y2
      (x1 to x2)
        .flatMap(x =>
          (y1 to y2).map(y => Point(x, y))
        )
    }
    if (dy.abs == dx.abs) diag() else straight()
  }

  def findOverlaps(points: Seq[Point]): Int = points
    .groupBy(identity)
    .map(d => (d._1, d._2.size))
    .count(d => d._2 >= 2)

  def part1(input: String): Int = {
    val lines = parseInput(input)
    val pointsCovered = lines
      .filterNot(isDiagonal)
      .flatMap(lineCovers)
    findOverlaps(pointsCovered)
  }

  def part2(input: String): Int = {
    val lines = parseInput(input)
    val points = lines
      .flatMap(lineCovers)
    findOverlaps(points)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: ${p1}\nPart2: ${p2}"
  }

  case class Point(x: Int, y2: Int)

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int)
}

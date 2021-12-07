package io.vltmn.aoc2021
package example

class Day7 extends Solution {
  def parseInput(input: String): Seq[Int] = input.split(",").map(_.toInt)

  def getMedian(vals: Seq[Int]): Int = {
    val sorted = vals.sorted
    val size = vals.size
    if (size % 2 == 0) sorted(size / 2) else (sorted((size / 2).floor.toInt) + sorted((size / 2).ceil.toInt)) / 2
  }

  def calcCost(data: Seq[Int], desiredPosition: Int): Int = data
    .map(_ - desiredPosition)
    .map(_.abs)
    .sum

  def calcIncreasingCost(data: Seq[Int], desiredPosition: Int): Int = data
    .map(_ - desiredPosition)
    .map(1 to _.abs)
    .map(_.sum)
    .sum

  def getAvg(vals: Seq[Int]): Int = (vals.sum / vals.size.toDouble).round.toInt

  def part1(input: String): Int = {
    val data = parseInput(input)
    val position = getMedian(data)
    calcCost(data, position)
  }

  def part2(input: String): Int = {
    val data = parseInput(input)
    val position = getAvg(data)
    val options = (-2 to 2)
      .map(_ + position)
      .map(calcIncreasingCost(data, _))
    options.min
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: ${p1}\nPart2: ${p2}"
  }
}

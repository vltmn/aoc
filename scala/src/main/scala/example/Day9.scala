package io.vltmn.aoc2021
package example

class Day9 extends Solution {
  def parseInput(input: String): (Seq[Int], Int) = {
    val arr = input
      .linesIterator
      .flatMap(l => (0 until l.length).map(i => l.charAt(i).toString).map(_.toInt))
      .toSeq
    val width = input.lines().findFirst().map(_.length).get()
    (arr, width)
  }

  def getEdgesIncluded(idx: Int, width: Int, totalSize: Int): (Boolean, Boolean, Boolean, Boolean) = {
    val topIncluded = (idx - width) >= 0
    val leftIncluded = (idx % width) != 0
    val rightIncluded = (idx % width) != width - 1
    val bottomIncluded = (idx + width) < totalSize
    (topIncluded, rightIncluded, bottomIncluded, leftIncluded)
  }

  def getIndexesPossible(idx: Int, width: Int, totalSize: Int): Seq[Int] = {
    val (topIncluded, rightIncluded, bottomIncluded, leftIncluded) = getEdgesIncluded(idx, width, totalSize)
    val edges = List(topIncluded, rightIncluded, bottomIncluded, leftIncluded)
    edges.indices
      .map {
        case 0 => (0, idx - width)
        case 1 => (1, idx + 1)
        case 2 => (2, idx + width)
        case 3 => (3, idx - 1)
      }
      .filter(d => edges(d._1))
      .map(_._2)
  }

  def isLowPoint(idx: Int, data: Seq[Int], width: Int): Boolean = {
    val (topIncluded, rightIncluded, bottomIncluded, leftIncluded) = getEdgesIncluded(idx, width, data.length)
    val pointValue = data(idx)
    (
      (!topIncluded || data(idx - width) > pointValue)
        &&
        (!leftIncluded || data(idx - 1) > pointValue)
        &&
        (!rightIncluded || data(idx + 1) > pointValue)
        &&
        (!bottomIncluded || data(idx + width) > pointValue)
      )

  }

  def part1(input: String): Int = {
    val (data, width) = parseInput(input)
    val lowPointIdxs = data.indices.filter(isLowPoint(_, data, width))
    lowPointIdxs.map(data(_) + 1).sum
  }

  def findBasinIdxs(idx: Int, data: Seq[Int], width: Int): Seq[Int] = data(idx) match {
    case 9 => List()
    case _ =>
      val possibleIdxs = getIndexesPossible(idx, width, data.length)
      val otherBasins = possibleIdxs
        .filter(data(_) > data(idx))
        .flatMap(findBasinIdxs(_, data, width))
      List(idx).concat(otherBasins).distinct.sorted
  }

  def part2(input: String): Int = {
    val (data, width) = parseInput(input)
    val lowPointIdxs = data.indices.filter(isLowPoint(_, data, width))

    lowPointIdxs
      .map(lpidx => findBasinIdxs(lpidx, data, width))
      .map(_.size)
      .sorted
      .reverse
      .take(3)
      .product
  }

  override def solve(input: String): String = {
    val p1 = part1(input);
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

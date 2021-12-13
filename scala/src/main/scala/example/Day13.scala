package io.vltmn.aoc2021
package example

class Day13 extends Solution {

  def parseInput(input: String): (Seq[Position], Seq[Fold]) = {
    def isFoldLine = (line: String) => line.startsWith("fold along")

    val positions = input.linesIterator
      .takeWhile(!isFoldLine(_))
      .filter(!_.isBlank)
      .map(l => l.split(",").map(_.toInt) match {
        case Array(x, y) => (x, y)
      })
      .toSeq
    val folds: Seq[Fold] = input.linesIterator
      .dropWhile(!isFoldLine(_))
      .map(l => l.split(" ").last.split("=") match {
        case Array(along, value) => new Fold(along.head, value.toInt)
      })
      .toSeq
    (positions, folds)
  }

  def doFold(positions: Seq[Position], fold: Fold): Seq[Position] = {
    def applyFold(value: Int, foldValue: Int): Int = {
      val delta = value - foldValue
      foldValue - delta
    }

    positions
      .map(p => fold.along match {
        case 'x' if p._1 > fold.value => (applyFold(p._1, fold.value), p._2)
        case 'y' if p._2 > fold.value => (p._1, applyFold(p._2, fold.value))
        case _ => (p._1, p._2)
      })
      .distinct
  }

  def part1(input: String): Int = {
    val (positions, fold :: _) = parseInput(input)
    val afterOneFold = doFold(positions, fold)
    afterOneFold.size
  }

  def prepareString(positions: Seq[Position]): String = {
    val height = positions.map(_._2).max
    val width = positions.map(_._1).max
    (0 to height)
      .map(y => (0 to width).map(x => (x, y))
        .map(loc => if (positions.contains(loc)) '#' else '.')
        .mkString
      ).foldLeft("")((acc, curr) => acc + "\n" + curr)
  }

  def part2(input: String): String = {
    val (positions, folds) = parseInput(input)
    val endPositions = folds.foldLeft(positions)((acc, curr) => doFold(acc, curr))
    prepareString(endPositions)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2:\n$p2"
  }

  type Position = (Int, Int)

  case class Fold(along: Char, value: Int)
}

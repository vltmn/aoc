package io.vltmn.aoc2021
package example

class Day11 extends Solution {
  type Cell = Int

  def parseInput(input: String): (Seq[Cell], Int) = {
    val board = input
      .linesIterator
      .flatMap(l => l.toCharArray)
      .map(c => c.toString.toInt)
      .toSeq
    val width = input.linesIterator.toSeq.head.length
    (board, width)
  }

  def getAdjacentIndices(idx: Int, width: Int, totalSize: Int): Seq[Int] = {
    val data = List(
      // top
      (idx - width) >= 0,
      // right
      (idx % width) != width - 1,
      // bottom
      (idx + width) < totalSize,
      // left
      (idx % width) != 0,
      // top-left
      (idx - width) >= 0 && (idx % width) != 0,
      // top-right
      (idx - width) >= 0 && (idx % width) != width - 1,
      // bottom-right
      (idx + width) < totalSize && (idx % width) != width - 1,
      // bottom-left
      (idx + width) < totalSize && (idx % width) != 0
    )
      .zipWithIndex
      .filter(_._1)
      .map(_._2)
      .map {
        case 0 => idx - width
        case 1 => idx + 1
        case 2 => idx + width
        case 3 => idx - 1
        case 4 => idx - width - 1
        case 5 => idx - width + 1
        case 6 => idx + width + 1
        case 7 => idx + width - 1
      }
    data
  }

  def flash(data: Seq[Cell], width: Int): Seq[Cell] = {
    val newBoard = data
      .indices
      .foldLeft(data.zipWithIndex)((acc, curr) => acc(curr)._1 match {
        case x if x > 9 => getAdjacentIndices(curr, width, data.length)
          // only apply to indices that haven't flashed
          .filter(idx => acc(idx)._1 != 0)
          // fold to increase adjacent indices
          .foldLeft(acc)((acc1, idx) => acc1.map(c =>
            if (c._2 == idx)
              (c._1 + 1, c._2)
            else
              c
          ))
          // set flashing idx to 0
          .map(d => if (d._2 == curr) (0, d._2) else d)
        case _ => acc
      })
      .map(_._1)
    // recurse if the board will continue to flash
    if (newBoard.exists(_ > 9)) flash(newBoard, width) else newBoard

  }

  def doStep(data: Seq[Cell], width: Int): Seq[Cell] = {
    val increased = data.map(_ + 1)
    flash(increased, width)
  }

  def part1(input: String): Int = {
    val (startPoint, width) = parseInput(input)
    val (flashes, endBoard) = (0 until 100).foldLeft((0, startPoint))((acc, _) => {
      val next = doStep(acc._2, width)
      (acc._1 + next.count(_ == 0), next)
    })
    flashes
  }

  def stepUntilAllFlashes(startPoint: Seq[Cell], width: Int): Int = {
    def innerFunction(board: Seq[Cell], width: Int, count: Int): Int = {
      val next = doStep(board, width)
      if (next.count(_ != 0) == 0)
        count + 1
      else
        innerFunction(next, width, count + 1)
    }
    innerFunction(startPoint, width, 0)
  }

  def part2(input: String): Int = {
    val (startPoint, width) = parseInput(input)
    stepUntilAllFlashes(startPoint, width)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)

    s"Part1: $p1\nPart2: $p2"
  }
}

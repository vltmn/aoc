package io.vltmn.aoc
package y2022

import util.MatrixImprovements.MatrixImprovements

class Day5 extends Solution {
  type Move = (Int, Int, Int)
  type Stack = Seq[Char]

  override def solve(input: String): String = {
    val splitted = input.split("\n\n")
    val moveRegex = "move (\\d+) from (\\d+) to (\\d+)".r
    val moves = moveRegex.findAllMatchIn(splitted.last)
      .map(m => (m.group(1).toInt, m.group(2).toInt - 1, m.group(3).toInt - 1))
      .toSeq

    val idxs = splitted.head.linesIterator.toSeq.last
      .zipWithIndex
      .filter(_._1.isDigit)
      .map(_._2)
      .toSet
    val stackLines = splitted.head.linesIterator.toSeq.dropRight(1)
      .map(l => l.zipWithIndex.filter(i => idxs.contains(i._2)).map(_._1))
    val stacks = stackLines
      .squarePad(' ')
      .transpose
      .map(_.filter(_.isLetter))

    def mkReducer(reverse: Boolean): (Seq[Stack], Move) => Seq[Stack] = (stacks, move) => {
      val toMove = stacks(move._2).take(move._1)
      stacks.zipWithIndex
        .map(s => s._2 match {
          case move._2 => stacks(move._2).drop(move._1)
          case move._3 => stacks(move._3).prependedAll(if (reverse) toMove.reverse else toMove)
          case _ => s._1
        })
    }

    val p1 = moves.foldLeft(stacks)(mkReducer(true)).map(_.head).mkString("")
    val p2 = moves.foldLeft(stacks)(mkReducer(false)).map(_.head).mkString("")
    s"Part1: $p1\nPart2: $p2"
  }
}

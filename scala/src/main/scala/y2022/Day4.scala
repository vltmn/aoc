package io.vltmn.aoc
package y2022

class Day4 extends Solution {
  override def solve(input: String): String = {
    val parsed = input.linesIterator
      .toSeq
      .map(_.split(",")
        .flatMap(_.split("-")
          .map(_.toInt))
        .toSeq
      )
      .map {
        case Seq(a, b, c, d) => (Range.Int(a, b + 1, 1).toSet, Range.Int(c, d + 1, 1).toSet)
      }
    val p1 = parsed
      .count {
        case (a, b) =>
          val intersect = a.intersect(b)
          intersect == a || intersect == b
      }
    val p2 = parsed
      .count {
        case (a, b) => a.intersect(b).nonEmpty
      }


    s"Part1: $p1\nPart2: $p2"
  }
}
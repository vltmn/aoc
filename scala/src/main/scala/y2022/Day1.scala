package io.vltmn.aoc
package y2022

class Day1 extends Solution {

  override def solve(input: String): String = {
    val deerSums = input.split("\\n\\n").map(s => s.linesIterator.map(_.toInt).sum)
    val p1 = deerSums.max
    val p2 = deerSums.sorted.takeRight(3).sum

    s"Part1: $p1\nPart2: $p2"
  }
}

package io.vltmn.aoc
package y2022

class Day6 extends Solution {
  override def solve(input: String): String = {
    def findDistinct(chars: Seq[Char], desiredSize: Int): Int = chars
      .sliding(desiredSize)
      .zipWithIndex
      .find(s => s._1.toSet.size == s._1.size)
      .get._2 + desiredSize

    val p1 = findDistinct(input, 4)
    val p2 = findDistinct(input, 14)

    s"Part1: $p1\nPart2: $p2"
  }
}

package io.vltmn.aoc
package y2022

class Day3 extends Solution {
  override def solve(input: String): String = {
    val lowerOffset = 96
    val upperOffset = 38
    val p1 = input.linesIterator
      .toSeq
      .map(_.split(""))
      .map(items => {
        val compSize = items.length / 2;
        (items.take(compSize), items.drop(compSize))
      })
      .map(comps => comps._1.intersect(comps._2))
      .map(_.head)
      .map(i => i.charAt(0))
      .map(c => if (c.isLower) c.toInt - lowerOffset else c.toInt - upperOffset)
      .sum
    val p2 = input.linesIterator
      .map(_.split(""))
      .map(_.toSet)
      .grouped(3)
      .map(g => g.head.intersect(g(1)).intersect(g(2)).head)
      .map(_.charAt(0))
      .map(c => if (c.isLower) c.toInt - lowerOffset else c.toInt - upperOffset)
      .sum
    s"Part1: $p1\nPart2: $p2"
  }
}

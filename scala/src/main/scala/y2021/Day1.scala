package io.vltmn.aoc
package y2021

class Day1 extends Solution {
  def partA(input: String): Int = input
    .linesIterator
    .map(_.toInt)
    .sliding(2)
    .count(s => s == s.sorted)

  def partB(input: String): Int = input
    .linesIterator
    .map(_.toInt)
    .sliding(3)
    .map(_.sum)
    .sliding(2)
    .count {
      case Seq(a, b) => a < b
    }
  override def solve(input: String): String = {
    val a = partA(input)
    val b = partB(input)
    a.toString ++ "\n" ++ b.toString
  }
}

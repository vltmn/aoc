package io.vltmn.aoc
package y2022

class Day2 extends Solution {
  // A, X rock
  // B, Y paper
  // C, Z scissors
  override def solve(input: String): String = {
    def mapToInt(s: String): Int = s match {
      case "A" | "X" => 0
      case "B" | "Y" => 1
      case "C" | "Z" => 2
    }

    val rounds = input.linesIterator.toSeq
      .map(l => {
        val s = l.split(" ")
        (s.head, s.last)
      })
      .map(p => (mapToInt(p._1), mapToInt(p._2)))

    val p1 = rounds
      .map(p => p._2 + 1 + (if (p._2 == p._1 + 1 || (p._2 == 0 && p._1 == 2)) 6 else if (p._2 == p._1) 3 else 0))
      .sum

    val p2 = rounds
      .map(p => p._2 match {
        case 0 => if (p._1 - 1 < 0) 3 else p._1
        case 1 => p._1 + 4
        case 2 => (p._1 + 1) % 3 + 7
      })
      .sum

    s"Part1: $p1\nP2: $p2"
  }
}

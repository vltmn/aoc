package io.vltmn.aoc
package y2022

class Day10 extends Solution {

  override def solve(input: String): String = {
    val parsed = input.linesIterator
      .flatMap(_ match {
        case str if str.startsWith("addx ") => Seq("noop", str)
        case str => Seq(str)
      })
      .map {
        case str if str.startsWith("addx") => str.split(" ").last.toInt
        case _ => 0
      }
      .toSeq
    val start = 20
    val increment = 40
    val counts = 6
    val scanned = parsed.scanLeft(1)((acc, curr) => acc + curr).dropRight(1)
    val p1 = (0 until counts)
      .map(_ * increment)
      .map(_ + start)
      .map(i => scanned(i - 1) * i).sum

    val p2 = scanned.grouped(40)
      .map(r => r
        .zipWithIndex
        .map(e => Math.abs(e._1 - e._2))
        .map(e => if (e <= 1) '#' else '.')
        .mkString
      )
      .mkString("\n")
    s"Part1: $p1\nPart2: \n$p2"
  }
}

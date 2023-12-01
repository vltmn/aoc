package io.vltmn.aoc
package y2023

import util.NumberUtils.DigitMap

class Day1 extends Solution {
  def sumLine(v: Seq[Seq[Int]]): Int = v.map(l => l.head * 10 + l.last).sum

  override def solve(input: String): String = {
    val l = input.linesIterator.toSeq

    val lInts = l.map(_.toCharArray.filter(_.isDigit)
      .map(_.toInt - 48).toSeq
    )
    val p1 = sumLine(lInts)

    val lletters = l.map(line => DigitMap.toSeq
        .foldLeft(line)((acc, curr) => acc.replaceAll(curr._1, curr._1 + curr._2 + curr._1))
      .filter(_.isDigit).map(_.toInt - 48))

    val p2 = sumLine(lletters)


    s"Part1: $p1\nPart2: $p2"
  }
}


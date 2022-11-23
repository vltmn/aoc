package io.vltmn.aoc
package y2021

class Day3 extends Solution {
  def getCharLength(input: String): Int = input
    .linesIterator
    .foldLeft(0)((acc, curr) => if (curr.length > acc) curr.length else acc)

  def part1(input: String): (Int, Int) = {
    val length = getCharLength(input)
    val xorOperand = Integer.parseInt("1".repeat(length), 2)
    val gammaBin = input
      .linesIterator
      .map(_.toCharArray)
      .toArray
      .transpose
      .map(bit => {
        val bitVal = bit.foldLeft(0)((acc, curr) => if (curr == '1') acc + 1 else acc - 1)
        if (bitVal > 0) '1' else '0'
      })
      .mkString

    val gamma = Integer.parseInt(gammaBin, 2);
    val epsilon = gamma ^ xorOperand
    (gamma, epsilon)
  }

  val bitCriteria: Boolean => Array[String] => Int => Array[String] = op => arr => i => {
    val mostCommon = if (arr
      .map(_.charAt(i).toString)
      .map(c => c.toInt)
      .sum > (arr.length - 1) / 2) '1' else '0'
    val matcher = if (op) (mostCommon.toInt ^ 1).toChar else mostCommon
    val filtered = arr.filter(s => s.charAt(i) == matcher)
    if (filtered.length == 1) filtered else bitCriteria(op)(filtered)(i + 1)
  }

  def part2(input: String): (Int, Int) = {
    val arr = input.linesIterator.toArray
    val oxygenRating = Integer.parseInt(bitCriteria(false)(arr)(0)(0), 2)
    val co2Rating = Integer.parseInt(bitCriteria(true)(arr)(0)(0), 2)
    (oxygenRating, co2Rating)
  }

  override def solve(input: String): String = {
    val (paGamma, paEpsilon) = part1(input)
    val (pbOxy, pbCO2) = part2(input)
    s"Part 1: ${paGamma * paEpsilon}\nPart 2: ${pbOxy * pbCO2}"
  }
}

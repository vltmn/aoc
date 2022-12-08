package io.vltmn.aoc
package y2022

class Day8 extends Solution {
  override def solve(input: String): String = {
    val matrix = input
      .linesIterator
      .map(l => l.split("").map(_.toInt).toSeq).toSeq
    val size = matrix.length
    val locs = (1 until size - 1).flatMap(i => (1 until size - 1).map(j => (j, i)))
    val outerCount = size * 4 - 4

    def getHiddenStates(loc: (Int, Int), matrix: Seq[Seq[Int]], transposed: Seq[Seq[Int]]): Seq[Boolean] = {
      val value = matrix(loc._2)(loc._1)

      val hiddenLeft = matrix(loc._2).slice(0, loc._1).exists(_ >= value)
      val hiddenRight = matrix(loc._2).slice(loc._1 + 1, size).exists(_ >= value)
      val hiddenTop = transposed(loc._1).slice(0, loc._2).exists(_ >= value)
      val hiddenBottom = transposed(loc._1).slice(loc._2 + 1, size).exists(_ >= value)
      Seq(hiddenLeft, hiddenTop, hiddenBottom, hiddenRight)
    }

    def calcScenicScore(loc: ((Int, Int), Seq[Boolean]), matrix: Seq[Seq[Int]], transposed: Seq[Seq[Int]]): Int = {
      val Seq(leftHidden, topHidden, bottomHidden, rightHidden) = loc._2
      val value = matrix(loc._1._2)(loc._1._1)

      val top = if (!topHidden) loc._1._2 else transposed(loc._1._1).slice(0, loc._1._2).reverse.takeWhile(_ < value).size + 1
      val left = if (!leftHidden) loc._1._1 else matrix(loc._1._2).slice(0, loc._1._1).reverse.takeWhile(_ < value).size + 1
      val right = if (!rightHidden) size - loc._1._1 - 1 else matrix(loc._1._2).slice(loc._1._1 + 1, size).takeWhile(_ < value).size + 1
      val bottom = if (!bottomHidden) size - loc._1._2 - 1 else transposed(loc._1._1).slice(loc._1._2 + 1, size).takeWhile(_ < value).size + 1
      left * right * top * bottom
    }

    val transposed = matrix.transpose
    val hiddenStates = locs.map(l => (l, getHiddenStates(l, matrix, transposed)))
    val notHiddenLocs = hiddenStates.filterNot(e => e._2.forall(e => e))
    val innerCount = notHiddenLocs.size
    val p1 = outerCount + innerCount

    val angles = 3
    val potentialScenicLocs = notHiddenLocs
      .filter(_._2.filterNot(e => e).size >= angles)
    val p2 = potentialScenicLocs.map(calcScenicScore(_, matrix, transposed)).max
    s"Part1: $p1\nPart2: $p2"
  }
}

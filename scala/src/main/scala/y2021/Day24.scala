package io.vltmn.aoc
package y2021


import scala.util.matching.Regex

class Day24 extends Solution {
  def findVars(input: String): Seq[(Int, Int, Int)] = {
    val pattern: Regex = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z (-?\\d*)\nadd x (-?\\d*)\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y (-?\\d*)\nmul y x\nadd z y".r
    pattern.findAllMatchIn(input)
      .toSeq
      .map(m => (m.group(1).toInt, m.group(2).toInt, m.group(3).toInt))
  }

  type RowVar = (Int, Int, Int)

  def genPairs(vars: Seq[RowVar], stack: Seq[RowVar], i: Int = 0): Seq[(RowVar, RowVar)] = vars match {
    case (1, a, b) :: rest =>
      genPairs(rest, stack.appended((i, a, b)), i + 1)
    case (26, a, b) :: rest =>
      val last = stack.last
      genPairs(rest, stack.dropRight(1), i + 1).prepended((last, (i, a, b)))
    case _ => Seq()
  }

  def findValWithpairs(pairs:  Seq[((Int, Int, Int), (Int, Int, Int))], mapOp: (((Int, Int), Int)) => ((Int, Int), (Int, Int))): Long = {
    val idxVals = pairs
      // map to number idxes, diff
      .map(p => ((p._1._1, p._2._1), p._1._3 + p._2._2))
      .map(mapOp)
      .flatMap(v => Seq((v._1._1, v._2._1), (v._1._2, v._2._2)))
      .toMap
    Range(0, 14)
      .map(idxVals.get)
      .filter(_.isDefined)
      .map(_.get)
      .map(i => i.toString)
      .fold("")((a, b) => a + b)
      .toLong
  }
  def part1(input: String): Long = {
    val vars = findVars(input)
    val pairs = genPairs(vars, Seq())
    def mapOp(v: ((Int, Int), Int)): ((Int, Int), (Int, Int)) = (v._1, if (v._2 < 0) (9, 9 + v._2) else (9 - v._2, 9))
    findValWithpairs(pairs, mapOp)
  }

  def part2(input: String): Long = {
    val vars = findVars(input)
    val pairs = genPairs(vars, Seq())
    def mapOp(v: ((Int, Int), Int)): ((Int, Int), (Int, Int)) = (v._1, if (v._2 < 0) (1 - v._2, 1) else (1, 1 + v._2))
    findValWithpairs(pairs, mapOp)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}
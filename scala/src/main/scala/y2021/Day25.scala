package io.vltmn.aoc
package y2021

import scala.annotation.tailrec

class Day25 extends Solution {

  type CucumberMap = Map[(Int, Int), Char]

  def parseInput(input: String): (Int, Int, CucumberMap) = {
    val width = input.linesIterator.next().length
    val height = input.linesIterator.toSeq.size
    val cucumbers = input.linesIterator
      .zipWithIndex
      // map to (x, y, v)
      .flatMap(v => v._1.zipWithIndex.map(z => (z._2, v._2, z._1)))
      .filter(e => e._3 != '.')
      .map(e => ((e._1, e._2), e._3))
      .toMap
    (height, width, cucumbers)
  }

  def step(cucumbers: CucumberMap, width: Int, height: Int): CucumberMap = {
    val newECucumbers = cucumbers
      .filter(e => e._2 == '>')
      .map(e => {
        val newPos = ((e._1._1 + 1) % width, e._1._2)
        if (cucumbers.contains(newPos)) e else (newPos, e._2)
      })
    val SCucumbers = cucumbers.filter(e => e._2 == 'v')
    val newCucumbers = SCucumbers ++ newECucumbers
    val newSCucumbers = SCucumbers
      .map(e => {
        val newPos = (e._1._1, (e._1._2 + 1) % height)
        if (newCucumbers.contains(newPos)) e else (newPos, e._2)
      })
    newSCucumbers ++ newECucumbers
  }

  def cucumberMapToString(cucumbers: CucumberMap, width: Int, height: Int): String =
    Range(0, height)
      .map(i => Range(0, width)
        .map[Char](j => cucumbers.getOrElse((j, i), '.'))
        .mkString
      )
      .mkString("\n")

  def part1(input: String): Int = {
    val (height, width, cucumbers) = parseInput(input)
    @tailrec
    def inner(cucumbers: CucumberMap, height: Int, width: Int, idx: Int = 1): Int = {
      if (idx % 5 == 0) println(idx)
      val next = step(cucumbers, width, height)
      if (next == cucumbers) idx else inner(next, height, width, idx + 1)
    }
    inner(cucumbers, height, width)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    s"Part1: $p1"
  }
}

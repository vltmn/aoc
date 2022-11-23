package io.vltmn.aoc
package y2021

class Day8 extends Solution {
  def parseInput(input: String): Seq[(Seq[String], Seq[String])] = input
    .linesIterator
    .map(l => {
      val tpl = l.split('|').map(_.trim)
      (tpl(0).split(" ").toSeq, tpl(1).split(" ").toSeq)
    })
    .toSeq

  def part1(input: String): Int = {
    val data = parseInput(input)
    val uniqueSizes = List(2, 3, 4, 7)
    data
      .map(_._2)
      .map(_.map(_.length).count(uniqueSizes.contains(_)))
      .sum
  }

  def buildWirings(charsLeft: Seq[Char]): Seq[String] = charsLeft
    .flatMap(c => {
      val nextChars = charsLeft.filter(_ != c)
      if (nextChars.nonEmpty)
        buildWirings(nextChars).map(c.toString + _)
      else
        List(c.toString)
    }).sorted

  def findWiring(input: Seq[String], allWirings: Seq[String], baseStr: String): String = {
    val sortedInput = input.sorted
      .map(_.sorted)
    allWirings.find(w => sortedInput.map(mapWiring(_, w, baseStr)).forall(_.isDefined)).get
  }

  val patterns: Seq[String] = List("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
    .map(_.sorted)

  def mapWiring(input: String, wiring: String, baseStr: String): Option[Int] = {
    val translated = input.map(c => baseStr.charAt(wiring.indexOf(c))).sorted
    val index = patterns.indexOf(translated)
    if (index == -1) None else Some(index)
  }

  def part2(input: String): Int = {
    val data = parseInput(input)
    val baseStr = "abcdefg"
    val wirings = buildWirings(baseStr)
    data
      .map(t => (findWiring(t._1, wirings, baseStr), t._2))
      .map(t => t._2
        .map(mapWiring(_, t._1, baseStr))
        .foldLeft("")((acc, curr) => acc + curr.map(_.toString).getOrElse("")).toInt)
      .sum
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

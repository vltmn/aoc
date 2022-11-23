package io.vltmn.aoc
package y2021

class Day10 extends Solution {
  val matchingChars: Map[Char, Char] = Map.from(List(
    ('(', ')'),
    ('{', '}'),
    ('[', ']'),
    ('<', '>')
  ))
  val unmatchingChars: Map[Char, Char] = matchingChars.toSeq
    .map(_.swap)
    .toMap

  def parseInput(input: String): Seq[Seq[Char]] = input
    .linesIterator
    .map(_.toCharArray.toSeq)
    .toSeq

  def matcher(tokens: List[Char], waitingToMatch: Seq[Char]): (Option[Char], Seq[Char]) = tokens match {
    case x :: xs if matchingChars.contains(x) =>
      matcher(xs, waitingToMatch :+ x)
    case x :: xs if unmatchingChars.contains(x) => {
      if (waitingToMatch.endsWith(unmatchingChars.get(x)))
        matcher(xs, waitingToMatch.dropRight(1))
      else
        (Some(x), waitingToMatch)
    }
    case Seq() => (None, waitingToMatch)
  }

  def scoreMapper(c: Char): Int = c match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  def part1(input: String): Int = {
    val data = parseInput(input)

    data
      .map(_.toList)
      .map(matcher(_, List()))
      .filter(_._1.isDefined)
      .map(_._1.get)
      .map(scoreMapper)
      .sum
  }

  def incompleteScoreMapper(c: Char): Int = c match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
  }

  def part2(input: String): Long = {
    val data = parseInput(input)
    val results = data.map(_.toList)
      .map(matcher(_, List()))
      .filter(_._2.nonEmpty)
      .filter(_._1.isEmpty)
      .map(_._2.reverse)
      .map(_
        .map(matchingChars.get)
        .map(_.get)
        .map(incompleteScoreMapper)
        .foldLeft(0L)((acc, curr) => acc * 5 + curr)
      )
      .sorted
    results(results.size / 2)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

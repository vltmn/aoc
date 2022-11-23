package io.vltmn.aoc
package y2021

class Day14 extends Solution {
  type Pair = (Char, Char)
  type Rules = Map[Pair, Char]
  type Pairs = Map[Pair, Long]

  def parseInput(input: String): (Seq[Pair], Rules) = {
    val indexedPairs = input.linesIterator.toSeq.head
      .zipWithIndex
    val pairs = indexedPairs
      .filter(_._2 != 0)
      .map(d => (indexedPairs(d._2 - 1)._1, d._1))

    val rules: Rules = input.linesIterator
      .dropWhile(!_.contains("->"))
      .map(l => {
        val parts = l.split("->").map(_.trim)
        val ps = parts(0).toCharArray
        ((ps(0), ps(1)), parts(1).toCharArray.head)
      })
      .toMap
    (pairs, rules)
  }

  def genPairs(data: Seq[Pair]): Pairs = data
    .groupBy(d => d)
    .map(entry => (entry._1, entry._2.size))

  def countOccurrences(outerChars: Seq[Char], pairs: Pairs): Seq[(Char, Long)] = pairs.toSeq
    .flatMap(e => List((e._1._1, e._2.toDouble / 2), (e._1._2, e._2.toDouble / 2)))
    .groupBy(_._1)
    .map(e => (e._1, e._2.map(_._2).sum))
    .map(e => (e._1, if (outerChars.contains(e._1)) Math.ceil(e._2) else e._2))
    .map(e => (e._1, e._2.toLong))
    .toSeq
    .sortBy(_._2)

  def performStep(data: Pairs, rules: Rules): Pairs = data
    .toSeq
    .flatMap(e => rules.get(e._1) match {
      case Some(newChar) => List(((e._1._1, newChar), e._2), ((newChar, e._1._2), e._2))
      case None => List(e)
    })
    .groupBy(_._1)
    .map(d => (d._1, d._2.map(_._2).sum))

  def part1(input: String): Long = {
    val (pairsSeq, rules) = parseInput(input)
    val pairs = genPairs(pairsSeq)
    val donePairs = (0 until 10).foldLeft(pairs)((acc, _) => performStep(acc, rules))
    val occurrences = countOccurrences(Seq(pairsSeq.head._1, pairsSeq.last._2), donePairs)
    occurrences.last._2 - occurrences.head._2
  }

  def part2(input: String): Long = {
    val (pairsSeq, rules) = parseInput(input)
    val pairs = genPairs(pairsSeq)
    val donePairs = (0 until 40)
      .foldLeft(pairs)((acc, _) =>
        performStep(acc, rules)
      )
    val occurrences = countOccurrences(Seq(pairsSeq.head._1, pairsSeq.last._2), donePairs)
    occurrences.last._2 - occurrences.head._2
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

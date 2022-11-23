package io.vltmn.aoc
package y2021

import y2021.Day21.{DeterministicDie, Die}

import scala.annotation.tailrec
import scala.collection.mutable

class Day21 extends Solution {
  def parseInput(input: String): Seq[Int] = input.linesIterator
    .map(l => l.split(":").last)
    .map(_.trim)
    .map(Integer.parseInt)
    .toSeq

  def playUntil(startingPositions: Seq[Int], playUntilScore: Long, die: Die): (Seq[Int], Seq[Long], Long) = {
    @tailrec
    def inner(positions: Seq[Int], scores: Seq[Long], count: Long): (Seq[Int], Seq[Long], Long) = {
      val pos :: restPos = positions
      val score :: restScore = scores
      val dieVal = die.roll() + die.roll() + die.roll()
      val nextPos = (pos + dieVal) % 10
      val nextScore = score + nextPos + 1
      if (nextScore >= playUntilScore)
        (restPos.appended(nextPos), restScore.appended(nextScore), count + 3)
      else
        inner(restPos.appended(nextPos), restScore.appended(nextScore), count + 3)
    }

    // index by 0 to 9 instead of 1 to 10
    val (endPos, endScore, rollCount) = inner(startingPositions.map(_ - 1), List.fill(startingPositions.size)(0), 0)
    (endPos.map(_ + 1), endScore, rollCount)
  }

  def playWithDiracDie(startingPositions: Seq[Int], playUntilScore: Long): Seq[Long] = {
    def nextPos(current: Int, dieVal: Int): Int = {
      val next = current + dieVal
      if (next % 10 == 0) 10 else next % 10
    }

    val allOutcomes = (1 to 3)
      .flatMap(a => (1 to 3).flatMap(b => (1 to 3).map(c => (a, b, c))))
      .map(e => e._1 + e._2 + e._3)
    val moveCounts = allOutcomes
      .groupBy(i => i)
      .map(e => (e._1, e._2.size))

    val cache = mutable.Map.empty[(Seq[Int], Seq[Int]), Seq[Long]]

    def inner(positions: Seq[Int], scores: Seq[Int]): Seq[Long] = cache.getOrElseUpdate((positions, scores), {
      if (scores.last >= playUntilScore)
        Seq.fill(scores.size - 1)(0L).appended(1L)
      else {
        moveCounts.foldLeft(Seq.fill(scores.size)(0L)) {
          case (wins, (dieVal, count)) =>
            val nextPosition = nextPos(positions.head, dieVal)
            val nextScore = scores.head + nextPosition
            val innerWins = inner(positions.drop(1).appended(nextPosition), scores.drop(1).appended(nextScore))
            wins.indices.map(i => wins(i) + innerWins(if (i == 0) innerWins.size - 1 else i - 1) * count)
        }

      }

    })

    inner(startingPositions, Seq.fill(startingPositions.size)(0))
  }


  def part1(input: String): Long = {
    val startingPositions = parseInput(input)
    val (_, scores, rollCount) = playUntil(startingPositions, 1000, new DeterministicDie)
    scores.min * rollCount
  }

  def part2(input: String): Long = {
    val startingPositions = parseInput(input)

    val results = playWithDiracDie(startingPositions, 21)
    results.max
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

object Day21 {
  trait Die {
    def roll(): Int
  }

  class DeterministicDie extends Die {
    private var nextValue = 1

    override def roll(): Int = {
      val toReturn = nextValue;
      nextValue += 1
      if (nextValue > 100)
        nextValue = 1
      toReturn
    }
  }
}
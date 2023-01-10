package io.vltmn.aoc
package y2022

import util.SeqUtils.Utils

import scala.collection.immutable.Queue

class Day20 extends Solution {
  type NumType = Long

  def mix(input: Seq[NumType], key: NumType = 1, times: Int = 1): Seq[NumType] = {
    val size = input.size
    val data = input.map(_ * key).zipWithIndex
    val q = Queue().appendedAll((0 until times).flatMap(_ => data))
    val maxMul = input.map(_.abs).max % (size - 1) + 1
    q.indices.foldLeft((data, q))((s, _) => {
      val d = s._1
      val q = s._2
      val (item, nextQ) = q.dequeue
      val idx = d.indexOf(item)
      val newIdxRaw = (idx + item._1) % (size - 1)
      val newIdx = if (newIdxRaw == 0) size - 1
      else if (newIdxRaw < 0) (0 until maxMul.toInt)
        .map(m => newIdxRaw + ((size - 1) * m))
        .dropWhile(_ < 0).head
      else
        newIdxRaw
      val newD = d.without(idx).withElem(newIdx.toInt, item)
      (newD, nextQ)
    })._1.map(_._1)
  }

  def extractCoords(data: Seq[NumType], idxs: Seq[Int]): NumType = idxs
    .map(_ + data.indexOf(0))
    .map(_ % data.size)
    .map(data(_))
    .sum

  override def solve(input: String): String = {
    val parsed = input.split("\n").map(_.toLong)
      .toSeq
    val groveCoords = Seq(1000, 2000, 3000)
    val p1 = extractCoords(mix(parsed), groveCoords)
    val decKey = 811589153L
    val p2 = extractCoords(mix(parsed, decKey, 10), groveCoords)
    s"Part1: \n$p1\nPart2: \n$p2"
  }
}

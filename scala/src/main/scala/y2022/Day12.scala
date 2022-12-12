package io.vltmn.aoc
package y2022

import util.MapUtils.Coord
import util.PathFinding.bfsLength

class Day12 extends Solution {
  def genGraph(matrix: Map[Coord, Int]): Set[(Coord, Coord)] = {
    val neighborDeltas = Seq((0, 1), (1, 0), (-1, 0), (0, -1))
    matrix.keySet
      .flatMap(k => {
        neighborDeltas
          .map(p => (k._1 + p._1, k._2 + p._2))
          .filter(matrix.contains)
          .filter(_ != k)
          .filter(p => matrix(p) - matrix(k) < 2)
          .map((k, _))
      })
  }

  override def solve(input: String): String = {
    val alphamatrix = input.linesIterator
      .map(_.split("").map(_.charAt(0))).toSeq

    val parsed = alphamatrix.map(_.map {
      case 'S' => 0
      case 'E' => 'z' - 97
      case c => c - 97
    }.toSeq)

    def findElem(v: Char): Coord = alphamatrix.zipWithIndex
      .map(e => (e._1.indexOf(v), e._2))
      .filter(e => e._1 != -1)
      .head

    val start = findElem('S')
    val end = findElem('E')

    val mapData: Map[Coord, Int] = parsed.map(_.indices)
      .zipWithIndex
      .flatMap(e => e._1.map((_, e._2)))
      .map(e => (e, parsed(e._2)(e._1)))
      .toMap
    val paths = genGraph(mapData)
    val succesors = paths.groupMap(_._1)(_._2)
    val predecessors = paths.groupMap(_._2)(_._1)

    val p1 = bfsLength(start, succesors(_), (p: Coord) => p == end).get

    val p2 = bfsLength(end, predecessors(_), (p: Coord) => parsed(p._2)(p._1) == 0).get
    s"Part1:\n$p1\nPart2:\n$p2"
  }
}

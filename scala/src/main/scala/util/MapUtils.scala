package io.vltmn.aoc
package util

import util.GraphTypes.{Edge, Graph}

object MapUtils {
  /*
  Coord is containing (x, y) coordinates
   */
  type Coord = (Int, Int)

  def genGraph(nodes: Set[Coord], passages: Set[Coord]): Graph[Coord] = {
    def inner(current: Coord, visited: Set[Coord] = Set()): Set[Edge[Coord]] = {
      val newNeighbours = passages.neigboursToSingleDim(current, excludeCoords = visited)
      newNeighbours match {
        case nn if nn.nonEmpty => {
          val ends = nn.intersect(nodes).map(e => (e, 1))
          val newVisited = visited.union(nn).incl(current)
          val nexts = nn.diff(nodes)
            .flatMap(n => inner(n, newVisited))
            .map(e => (e._1, e._2 + 1))
          ends.union(nexts)
        }
        case _ => Set()
      }
    }

    nodes.map(n => (n, inner(n)))
      .toMap
  }

  implicit class CoordUtils(val c1: Coord) {
    def compare(c2: Coord): Int = if (c1._1 < c2._1 || c1._2 < c2._2)
      -1
    else if (c1 == c2)
      0
    else
      1
  }

  implicit class MatrixMapUtils[A](val m: Seq[Seq[A]]) {
    def toMapCoord(filter: A => Boolean = _ => true): Map[Coord, A] = m
      .zipWithIndex
      .flatMap(e => e._1
        .zipWithIndex
        .filter(a => filter(a._1))
        .map(a => ((a._2, e._2), a._1))
      )
      .toMap
  }

  implicit class CoordSetUtils(val m: Set[Coord]) {
    private val neighborCoords = (-1 to 1).flatMap(i => (-1 to 1).map(j => (i, j))).toSet

    def neighboursTo(c: Coord, includeSelf: Boolean = false, excludeCoords: Set[Coord] = Set()): Set[Coord] = {
      val coords = (if (includeSelf) neighborCoords else neighborCoords.filter(e => !(e._1 == 0 && e._2 == 0)))
        .map(l => (c._1 + l._1, c._2 + l._2)).diff(excludeCoords)
      m.intersect(coords)
    }

    val singleDimNeighbourCoords = Set((-1, 0), (1, 0), (0, -1), (0, 1))
    def neigboursToSingleDim(c: Coord, excludeCoords: Set[Coord] = Set()): Set[Coord] =
      singleDimNeighbourCoords.map[Coord](l => (c._1 + l._1, c._2 + l._2))
        .diff(excludeCoords)
        .intersect(m)
  }

  implicit class MapCoordUtils[A](val m: Map[Coord, A]) {
    def neighboursTo(c: Coord, includeSelf: Boolean = false, excludeCoords: Set[Coord] = Set()): Map[Coord, A] = {
      val keys: Set[Coord] = m.keySet
        .neighboursTo(c, includeSelf, excludeCoords)
      m.filter(e => keys.contains(e._1))
    }
  }
}

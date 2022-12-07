package io.vltmn.aoc
package y2019

import util.MapUtils.{CoordUtils, MapCoordUtils, MatrixMapUtils, genGraph}

import io.vltmn.aoc.util.Dijkstra
import io.vltmn.aoc.util.GraphTypes.{Graph, GraphUtils}

class Day20 extends Solution {
  override def solve(input: String): String = {
    val inp2 = "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       "
    val cells = input.linesIterator
      .map(_.toCharArray.toSeq)
      .toSeq

    val coordMap = cells.toMapCoord(c => c.isLetter || c == '.')
    val passages = coordMap.filter(e => e._2 == '.')
    // get portals
    val labeledCoords = coordMap.filter(e => e._2.isLetter)
    val labelsLocation = labeledCoords.groupMapReduce(c => {
      // map to letter pair in sorted order
      val other = labeledCoords.neighboursTo(c._1).head
      val cmp = c._1.compare(other._1)
      if (cmp < 0) s"${c._2}${other._2}" else s"${other._2}${c._2}"
    })(e => passages.neighboursTo(e._1))((a, b) => a.concat(b))
      .view.mapValues(e => e.keys.toSet)

    val nodes = labelsLocation.toSeq
      .flatMap(e => e._2.map(ee => (e._1, ee)))
    val start = labelsLocation("AA").head
    val end = labelsLocation("ZZ").head
    // create graph with shortest path between nodes(everything not walls or empty spaces, e.g. labels
    val graph = genGraph(nodes.map(_._2).toSet, passages.keySet)
    val teleports: Graph[(Int, Int)] = labelsLocation
      .filter(_._2.size == 2).values
      .flatMap(e => Seq((e.head, Set((e.last, 1))), (e.last, Set((e.head, 1)))))
      .toMap
    val graphWithTeleports = graph.merge(teleports)
      //.flatMap(e => Seq(e(0)))
    // get shortest path from start to end
    val p1 = Dijkstra.shortestPath(graphWithTeleports, start, end)

    s"Part1: $p1"
  }
}

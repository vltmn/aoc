package io.vltmn.aoc
package y2022

import util.Dijkstra

import scala.collection.mutable

class Day16 extends Solution {
  type ValveId = String

  case class Valve(name: ValveId, rate: Int, tunnels: Seq[ValveId])

  def toGraphviz(parsed: Seq[Valve]): String = {
    val nodes = parsed.map(p => s"${p.name} [xlabel=${p.rate}];")
      .mkString("\n")
    val paths = parsed.flatMap(e => e.tunnels.map((e.name, _)))
      .map(e => if (e._1 > e._2) e else e.swap)
      .toSet
      .map((p: (ValveId, ValveId)) => s"${p._1} -- ${p._2};")
      .mkString("\n")
    s"graph {\n$nodes\n$paths\n}"
  }

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  override def solve(input: String): String = {
    val parsed = input
      .linesIterator
      .map(l => {
        val name = l.slice(6, 8)
        val rate = l.split("=").last.split(";").head.toInt
        val tunnels = l.drop(49).split(",").map(_.trim)
        Valve(name, rate, tunnels)
      })
      .toSeq

    val valveFlows = parsed.map(v => (v.name, v.rate)).toMap
    val nodes = parsed
      .map(v => {
        val edges = v.tunnels.map(t => (t, 1)).toSet
        if (v.rate == 0)
          (v.name, edges)
        else
          (v.name, edges.incl((v.name + "'", 1)))
      })
    // make every non zero valve have a friend valve with negative cost of the
    val nonZeroValves = parsed
      .filter(v => v.rate > 0)
      .map(v => (v.name + "'", Set((v.name, 1))))
    val nonZeroValveIds = nonZeroValves.map(_._1.dropRight(1)).toSet
    val graph = nodes.appendedAll(nonZeroValves).toMap
    val flowValvePairs = nonZeroValveIds.incl("AA")
      .subsets(2)
      .map(s => s.toSeq.sorted)
      .map(s => (s.head, s.last))
      .toSet
    val sps = flowValvePairs.map(fvp => (fvp, Dijkstra.shortestPath(graph, fvp._1, fvp._2 + "'").get))

    val spPaths = sps
      .flatMap(sp => Seq(sp, (sp._1.swap, sp._2)))
      .toMap

    lazy val calcMaxReward: ((ValveId, Set[ValveId], Int)) => Int = memoize {
      case (current, remaining, remainingMins) => remainingMins match {
        case i if i <= 0 => 0
        case _ =>
          val contribution = valveFlows(current) * remainingMins
          val remainingValves = remaining.excl(current)
            .map(rv => (rv, spPaths(current, rv)))
            .filter(_._2 <= remainingMins)
          val remainingValveIds = remainingValves.map(_._1)
          if (remainingValves.isEmpty)
            contribution
          else {
            val maxNext = remainingValves
              .map(i => calcMaxReward(i._1, remainingValveIds, remainingMins - i._2))
              .max
            contribution + maxNext
          }
      }
    }

    val p1 = calcMaxReward(("AA", nonZeroValveIds, 30))

    val combinations = (1 to nonZeroValveIds.size / 2)
      .flatMap(nonZeroValveIds.subsets)
      .map(ss => nonZeroValveIds.partition(ss.contains))
    val p2 = combinations
      .map(c => calcMaxReward(("AA", c._1, 26)) + calcMaxReward(("AA", c._2, 26)))
      .max

    s"Part1:\n$p1\nPart2:\n$p2"
  }
}

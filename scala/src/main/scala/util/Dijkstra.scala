package io.vltmn.aoc2021
package util

import scala.collection.mutable

object Dijkstra {
  case class Path[Pos](to: Pos, distance: Int)

  type Graph[Pos] = Map[Pos, Set[Path[Pos]]]

  def shortestPath[Pos](graph: Graph[Pos], source: Pos, target: Pos): Int = {
    val dist = collection.mutable.Map.from(graph
      .map(e => if (e._1 == source) (source, 0) else (e._1, Int.MaxValue)))
    val prev = collection.mutable.HashMap.empty[Pos, Pos]
    val ordering: Ordering[(Pos, Int)] = Ordering.by(_._2)
    val q: mutable.PriorityQueue[(Pos, Int)] = collection.mutable.PriorityQueue.from(
      graph.keySet
        .map(k => (k, dist(k)))
    )(ordering.reverse)

    while (q.nonEmpty) {
      val (u, _) = q.dequeue
      for (v <- graph(u)) {
        val newDist = dist(u) + v.distance
        if (newDist < dist(v.to)) {
          dist.update(v.to, newDist)
          prev.update(v.to, u)
          q.enqueue((v.to, newDist))
        }
      }
    }
    dist(target)
  }

}

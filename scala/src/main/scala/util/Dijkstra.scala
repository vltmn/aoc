package io.vltmn.aoc
package util

import io.vltmn.aoc.util.GraphTypes.Graph

import scala.collection.mutable

object Dijkstra {
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
        val newDist = dist(u) + v._2
        if (newDist < dist(v._1)) {
          dist.update(v._1, newDist)
          prev.update(v._1, u)
          q.enqueue((v._1, newDist))
        }
      }
    }
    dist(target)
  }

}

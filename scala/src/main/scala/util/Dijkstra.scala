package io.vltmn.aoc
package util

import util.GraphTypes.Graph

import scala.annotation.tailrec
import scala.collection.SortedMap

object Dijkstra {
  private type QueueType[Pos] = SortedMap[Int, Set[Pos]]
  def shortestPath[Pos](graph: Graph[Pos], source: Pos, target: Pos): Option[Int] = {

    @tailrec
    def inner(queue: QueueType[Pos], dists: Map[Pos, Int], prev: Map[Pos, Pos]): Option[Int] = {
      if (queue.isEmpty) return dists.get(target)
        .flatMap(i => if (i == Int.MaxValue) None else Some(i))
      val item = queue.head._2.head
      val newItems = graph(item)
        .map(e => (e, dists(item) + e._2))
        .filter(e => e._2 < dists(e._1._1))
      val newDist = dists ++ newItems.map(e => (e._1._1, e._2))
      val newPrev = prev ++ newItems.map(e => (e._1._1, item))
      val poppedQ = queue.map(e => (e._1, e._2.diff(Set(item))))
        .filter(e => e._2.nonEmpty)
      val newQ = newItems.map(e => (e._2, e._1._1))
        .foldLeft(poppedQ)((acc, curr) =>
          acc.concat(Map((curr._1, acc.getOrElse(curr._1, Set[Pos]()).incl(curr._2))))
        )
      inner(newQ, newDist, newPrev)
    }
    val dists = graph.keySet
      .map(k => (k, if (k == source) 0 else Int.MaxValue))
      .toMap
    val startQueue = SortedMap from dists
      .map(_.swap)
      .groupMap(_._1)(_._2)
      .map(e => (e._1, e._2.toSet))

    inner(startQueue, dists, Map())
  }

}

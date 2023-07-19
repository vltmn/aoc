package io.vltmn.aoc
package util

import io.vltmn.aoc.util.GraphTypes.Graph

import scala.annotation.tailrec

object PathFinding {
  @tailrec
  private def reassemblePath[Pos](start: Pos, parents: Map[Pos, Pos], current: Pos, acc: List[Pos]): List[Pos] = {
    val newAcc = current :: acc
    if (current == start) newAcc
    else reassemblePath(start, parents, parents(current), newAcc)
  }


  private def genLoop[Pos](start: Pos, successors: Pos => Set[Pos], isGoal: Pos => Boolean, queueAppender: (Set[Pos], Seq[Pos]) => Seq[Pos]): () => Option[Seq[Pos]] = {
    @tailrec
    def inner(queue: Seq[Pos] = Seq(start), visited: Set[Pos] = Set(), parents: Map[Pos, Pos] = Map()): Option[Seq[Pos]] = queue match {
      case x :: xs =>
        x match {
          case x if isGoal(x) => Some(reassemblePath(start, parents, x, Nil))
          case _ =>
            val toAdd = successors(x).diff(visited)
            inner(queueAppender(toAdd, xs), visited.union(toAdd), parents.concat(toAdd.map((_, x))))
        }
      case _ => None
    }

    () => inner()
  }

  def bfsLength[Pos](start: Pos, successors: Pos => Set[Pos], isGoal: Pos => Boolean): Option[Int] =
    bfs(start, successors, isGoal).map(_.length - 1)

  def bfs[Pos](start: Pos, successors: Pos => Set[Pos], isGoal: Pos => Boolean): Option[Seq[Pos]] =
    genLoop(start, successors, isGoal, (toAdd: Set[Pos], q: Seq[Pos]) => q.appendedAll(toAdd))()

  def bfs[Pos](start: Pos, graph: Graph[Pos], isGoal: Pos => Boolean): Option[Seq[Pos]] =
    bfs(start, toSuccessors(graph), isGoal)
  private def toSuccessors[Pos](g: Graph[Pos]): Pos => Set[Pos] = g.view
    .mapValues(v => v.map(_._1)).toMap
  def dfsLength[Pos](
                      start: Pos,
                      successors: Pos => Set[Pos],
                      isGoal: Pos => Boolean,
                    ): Option[Int] =
    dfs(start, successors, isGoal).map(_.length - 1)

  def dfs[Pos](start: Pos, successors: Pos => Set[Pos], isGoal: Pos => Boolean): Option[Seq[Pos]] =
    genLoop(start, successors, isGoal, (toAdd: Set[Pos], q: Seq[Pos]) => q.prependedAll(toAdd))()
}


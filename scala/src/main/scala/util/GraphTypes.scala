package io.vltmn.aoc
package util



object GraphTypes {
  /*
  An edge with a specified cost
   */
  type Edge[E] = (E, Int)

  type Graph[E] = Map[E, Set[Edge[E]]]

  implicit class GraphUtils[E](val g: Graph[E]) {
    def merge(other: Graph[E]): Graph[E] = {
      g ++ other.map(e => (e._1, g.getOrElse(e._1, Set()).union(e._2)))
    }
  }
}

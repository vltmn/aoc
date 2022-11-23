package io.vltmn.aoc
package y2021

import scala.collection.mutable

class Day15 extends Solution {
  case class Position(x: Int, y: Int)

  case class Path(to: Position, distance: Int)

  type Graph = Map[Position, Set[Path]]

  def getAdjacentIndices(pos: Position, width: Int, height: Int): Seq[Position] = {
    val xs = List(if (pos.x % width == 0) None else Some(-1), if (pos.x % width == width - 1) None else Some(1))
    val ys = List(if (pos.y == 0) None else Some(-1), if (pos.y == height - 1) None else Some(1))
    List(
      xs.head.map(Position(_, 0)),
      xs.last.map(Position(_, 0)),
      ys.head.map(Position(0, _)),
      ys.last.map(Position(0, _))
    ).filter(o => o.isDefined)
      .map(o => o.get)
      .map(p => Position(pos.x + p.x, pos.y + p.y))
  }

  def parseInput(input: String): Seq[Seq[Int]] =
    input.linesIterator
      .map(_.toCharArray.map(_.asDigit).toSeq)
      .toSeq

  def asGraph(data: Seq[Seq[Int]]): Graph = {
    val width = data.length
    val height = data.transpose.length
    (0 until width).flatMap(x => (0 until height)
      .map(y =>
        Position(x, y)))
      .map(pos => (
        pos,
        Set.from(getAdjacentIndices(pos, width, height).map(adj => Path(adj, data(adj.y)(adj.x))))
      ))
      .toMap
  }

  def shortestPath(graph: Graph, source: Position, target: Position): Int = {
    val dist = collection.mutable.Map.from(graph
      .map(e => if (e._1 == source) (source, 0) else (e._1, Int.MaxValue)))
    val prev = collection.mutable.HashMap.empty[Position, Position]
    val ordering: Ordering[(Position, Int)] = Ordering.by(_._2)
    val q: mutable.PriorityQueue[(Position, Int)] = collection.mutable.PriorityQueue.from(
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

  def part1(input: String): Int = {
    val data = parseInput(input)
    val graph = asGraph(data)
    val cost = shortestPath(graph, Position(0, 0), Position(data.length - 1, data.transpose.length - 1))

    cost
  }

  def expandData(data: Seq[Seq[Int]], count: Int): Seq[Seq[Int]] = {
    val size = data.size
    val changes = (0 until count).map(i => (0 until count).map(j => i + j))

    (0 until size * count).map(y => (0 until size * count).map(x => {
      val elem = data(y % size)(x % size)
      val delta = changes(y / size)(x / size)
      if (elem + delta > 9)
        (elem + delta) % 9
      else
        elem + delta
    }))
  }

  def part2(input: String): Int = {
    val preData = parseInput(input)
    val data = expandData(preData, 5)
    val graph = asGraph(data)

    val cost = shortestPath(graph, Position(0, 0), Position(data.length - 1, data.transpose.length - 1))

    cost
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + ((t1 - t0).toDouble / Math.pow(10, 6)).round + "ms")
    result
  }

  override def solve(input: String): String = {

    val p1 = time {
      part1(input)
    }

    val p2 = time {
      part2(input)
    }
    s"Part1: $p1\nPart2: $p2"
  }
}

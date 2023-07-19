package io.vltmn.aoc
package y2022

import util.GraphTypes.{Graph, GraphUtils}
import util.PathFinding.bfs
import util.Position

import scala.annotation.tailrec

class Day24 extends Solution {
  trait Direction {
    def score(): Int

    def delta(): Position = {
      val vertical = score() % 2 == 0
      val positive = score() == 1 || score() == 2
      val p = (if (positive) 1 else -1, 0)
      Position(if (vertical) p.swap else p)
    }

    def char(): Char = score() match {
      case 0 => '^'
      case 1 => '>'
      case 2 => 'v'
      case 3 => '<'
    }
  }

  object Direction {
    case class North() extends Direction {
      override def score(): Int = 0
    }

    case class East() extends Direction {
      override def score(): Int = 1
    }

    case class South() extends Direction {
      override def score(): Int = 2
    }

    case class West() extends Direction {
      override def score(): Int = 3
    }

    val NORTH = North()
    val EAST = East()
    val SOUTH = South()
    val WEST = West()

    def parse(c: Char): Option[Direction] = c match {
      case '^' => Some(NORTH)
      case '>' => Some(EAST)
      case 'v' => Some(SOUTH)
      case '<' => Some(WEST)
      case _ => None
    }

  }

  type Blizzard = (Position, Direction)

  def parseMap(str: String): (Set[Blizzard], Position, Position) = {
    val poses = str.split("\n")
      .zipWithIndex.flatMap(e => e._1.toCharArray.zipWithIndex.map(ee => (ee._2, e._2, ee._1)))
    val blizzards = poses
      .map(e => Direction.parse(e._3).map(d => (Position(e._1, e._2), d)))
      .filter(_.nonEmpty).map(_.get).toSet
    val start = Position(1, 0)
    val lastPoint = poses.filter(_._3 == '.')
      .maxBy(_._2)
    val end = Position(lastPoint._1, lastPoint._2)
    (blizzards, start, end)
  }

  def genBlizzardPositions(initBlizzards: Set[Blizzard]): Seq[Set[Blizzard]] = {
    val xMin = 1
    val xMax = initBlizzards.map(_._1.x).max
    val yMin = 1
    val yMax = initBlizzards.map(_._1.y).max

    def wrap(v: Int, min: Int, max: Int): Int = if (v < min) max else if (v > max) min else v

    def moveBlizzards(blizzards: Set[Blizzard]): Set[Blizzard] = {
      blizzards
        // add the direction
        .map(e => (e._1 + e._2.delta(), e._2))
        // wrap around
        .map(e => (Position(wrap(e._1.x, xMin, xMax), wrap(e._1.y, yMin, yMax)), e._2))

    }

    @tailrec
    def inner(current: Set[Blizzard], visited: Seq[Set[Blizzard]]): Seq[Set[Blizzard]] = {
      val moved = moveBlizzards(current)
      assert(current.size == moved.size)
      if (visited.headOption.exists(_ equals moved))
        visited.appended(current)
      else
        inner(moved, visited.appended(current))
    }

    inner(initBlizzards, Seq())
  }

  case class GraphNodeType(minute: Int, pos: Position)

  def genGraph(emptySpaces: Seq[Set[Position]], start: Position, end: Position): Graph[GraphNodeType] = {
    // go through every minute in poses and generate graph for that, then merge the graphs
    val possibleMoves = Seq(Direction.NORTH, Direction.SOUTH, Direction.EAST, Direction.WEST)
      .map(_.delta()).toSet
      // include the wait position
      .incl(Position(0, 0))

    val spaceMins = emptySpaces
      // add the start and end positions, they are always empty
      .map(_.union(Set(start, end)))
      .zipWithIndex
    // append the first one to make the last minute have edges
    spaceMins.appended(spaceMins.head)
      .sliding(2).map(e => {
      val curr = e.head
      val nextSpaces = e.last._1
      val nextMin = e.last._2
      val min = curr._2
      curr._1.map(p => {
        val edges = possibleMoves.map(_ + p).intersect(nextSpaces)
        (GraphNodeType(min, p), edges.map(e => (GraphNodeType(nextMin, e), 1)))
      }).toMap
    }).reduce(_ merge _)
  }

  def getEmptySpaces(blizzards: Seq[Set[(Position, Direction)]]): Seq[Set[Position]] = {
    val allBlizzardPoses = blizzards.flatten.map(_._1)
    val xMin = allBlizzardPoses.map(_.x).min
    val xMax = allBlizzardPoses.map(_.x).max
    val yMin = allBlizzardPoses.map(_.y).min
    val yMax = allBlizzardPoses.map(_.y).max
    val allPoses = Range.inclusive(yMin, yMax).flatMap(y => Range.inclusive(xMin, xMax).map(x => (x, y)))
      .map(p => Position(p._1, p._2)).toSet
    blizzards.map(bs => allPoses.diff(bs.map(_._1)))
  }

  override def solve(input: String): String = {
    val (blizzards, startPos, endPos) = parseMap(input)
    val possibleBlizzardPoses = genBlizzardPositions(blizzards)
    val emptySpaces = getEmptySpaces(possibleBlizzardPoses)
    val graph = genGraph(emptySpaces, startPos, endPos)
    val path1 = bfs(GraphNodeType(0, startPos), graph, (gnt: GraphNodeType) => gnt.pos == endPos).get

    val path2 = bfs(path1.last, graph, (gnt: GraphNodeType) => gnt.pos == startPos).get
    val path3 = bfs(path2.last, graph, (gnt: GraphNodeType) => gnt.pos == endPos).get
    val p1 = path1.size - 1
    val p2 = path1.size - 1 + path2.size - 1 + path3.size - 1
    s"P1: $p1\nP2: $p2"
  }
}

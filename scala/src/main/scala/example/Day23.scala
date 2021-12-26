package io.vltmn.aoc2021
package example

import example.Day23.{Position, SpaceContent, State}
import util.Dijkstra
import util.Dijkstra.Path

class Day23 extends Solution {
  def parseInput(input: String): State = {
    def withoutPadding = input.linesIterator
      .filter(l => l.count(_ == '#') != l.length)
      .map(l => l.drop(1).dropRight(1))
      .toSeq

    val hallSpaces: Seq[(Position, SpaceContent)] = withoutPadding.head
      .zipWithIndex
      .map(e => (Position(e._2, 0, isHallway = true), SpaceContent(e._1)))
    val rooms: Seq[(Position, SpaceContent)] = withoutPadding
      .zipWithIndex
      .tail
      .flatMap(e => e._1
        .zipWithIndex
        .filter(_._1 != '#')
        .filter(_._1 != ' ')
        .map(ee => (Position(ee._2, e._2, isHallway = false), SpaceContent(ee._1)))
      )
    val map = hallSpaces.appendedAll(rooms)
      .toMap
    State(map)
  }

  def evalForGraph(startState: State): Map[State, Set[Path[State]]] = {

    val mutGraph = scala.collection.mutable.Map[State, Set[Path[State]]]()
    val toVisit = scala.collection.mutable.Queue[State]()
    toVisit.addOne(startState)
    while (toVisit.nonEmpty) {
      val state = toVisit.dequeue()

      val neighbours = state.getNeighbors
      val paths = neighbours
        .map(e => Path(e._1, e._2))
        .toSet
      mutGraph.addOne(state, paths)
      toVisit.addAll(neighbours.keySet.diff(mutGraph.keySet))
      if (toVisit.size > 300000) {
        val toAdd = toVisit.toSet.diff(mutGraph.keySet)
        toVisit.clear()
        toVisit.enqueueAll(toAdd)
      }

    }

    mutGraph.toMap
  }

  def getTargetState(startState: State): State = {
    val hallways = startState.map
      .filter(_._1.isHallway)
      .map(e => (e._1, SpaceContent('.')))
    val currentRooms = startState.map
      .filterNot(_._1.isHallway)
    val roomXs = currentRooms
      .map(_._1.x)
      .toVector
      .distinct
      .sorted
    val nextRooms = currentRooms
      .map(e => (e._1, SpaceContent(roomXs.indexOf(e._1.x) match {
        case 0 => 'A'
        case 1 => 'B'
        case 2 => 'C'
        case 3 => 'D'
      })))
    State(hallways ++ nextRooms)
  }

  def part1(input: String): Int = {
    val startState = parseInput(input)
    val target = getTargetState(startState)
    val graph = evalForGraph(startState)
    val cost = Dijkstra.shortestPath(graph, startState, target)
    cost
  }

  def part2(input: String): Int = {
    val lines = input.linesIterator.toSeq
    val p2Input = lines.take(3) ++ Seq("  #D#C#B#A#", "  #D#B#A#C#") ++ lines.drop(3)
    val startState = parseInput(p2Input.mkString("\n"))
    val target = getTargetState(startState)
    val graph = evalForGraph(startState)
    val cost = Dijkstra.shortestPath(graph, startState, target)
    cost
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

object Day23 {
  type SpaceContent = Option[Amphipod]

  case class Amphipod(kind: String, cost: Int)

  object SpaceContent {
    def apply(c: Char): SpaceContent = c match {
      case '.' => None
      case 'A' => Some(Amphipod("Amber", 1))
      case 'B' => Some(Amphipod("Bronze", 10))
      case 'C' => Some(Amphipod("Copper", 100))
      case 'D' => Some(Amphipod("Desert", 1000))
    }
  }

  case class Position(x: Int, y: Int, isHallway: Boolean)

  case class State(map: Map[Position, SpaceContent]) {
    def getNeighbors: Map[State, Int] = {
      val occupiedRooms = map
        .filter(_._2.isDefined)
        .keySet
        .filterNot(_.isHallway)

      val orderedRoomXs = roomXs.toVector.sorted

      val fromRoomsToHallway = map
        .filterNot(_._1.isHallway)
        .filter(_._2.isDefined)
        // there is no amphipod in room "in front of"
        .filterNot(e => occupiedRooms
          .filter(_.x == e._1.x)
          .filter(_ != e._1)
          .map(_.y).exists(_ < e._1.y)
        )
        // there is an invalid amphipod below this one
        .filter(e => {
          val isInRightRoom = amphipodIdxMatcher(e._2.get.kind) == orderedRoomXs.indexOf(e._1.x)
          !isInRightRoom || (isInRightRoom && map
            .filter(me => me._2.isDefined)
            .filter(me => me._2.get != e._2.get)
            .filter(me => me._1.x == e._1.x)
            .exists(me => me._1.y > e._1.y))
        }
        )
        .flatMap(e => neighborMapper(e._1, e._2.get, hallwayPositionsReachable))

      val fromHallways = map
        .filter(_._1.isHallway)
        .filter(_._2.isDefined)
        .flatMap(e => neighborMapper(e._1, e._2.get, roomsReachable))

      fromRoomsToHallway ++ fromHallways
    }

    override def toString: String = {
      val maxX = map.keys.map(_.x).max
      val maxY = map.keys.map(_.y).max
      val lines = (0 to maxY).map(y => (0 to maxX).map(x => map
        .get(Position(x, y, y == 0))
        .map(_.map(_.kind.head).getOrElse('.')).getOrElse(' ')).mkString)
      lines.mkString("\n")
    }

    private def neighborMapper(position: Position, amphipod: Amphipod, reachableFn: (Position, Amphipod) => Seq[Position]): Map[State, Int] =
      reachableFn(position, amphipod)
        .map(rp => (
          State(map
            .updated(position, None)
            .updated(rp, Some(amphipod))
          ),
          distance(position, rp) * amphipod.cost
        ))
        .toMap

    private def hallwayPositionsReachable(position: Position, pod: Amphipod): Seq[Position] = {
      val freeHallwayPositions = map
        .filter(_._1.isHallway)
        .filterNot(_._2.isDefined)
        .filterNot(e => forbiddenHallwayPositions.contains(e._1))
      val occupiedHallwayXs = map
        .filter(_._1.isHallway)
        .filter(_._2.isDefined)
        .map(e => e._1.x)
        .toSeq
      // filter out free positions where there is an occupied x between position and free position
      freeHallwayPositions
        .keys
        .filterNot(fp => {
          val minX = fp.x.min(position.x)
          val maxX = fp.x.max(position.x)
          occupiedHallwayXs.exists(x => minX < x && x < maxX)
        })
        .toSeq
    }

    private def roomsReachable(position: Position, amphipod: Amphipod): Seq[Position] = {

      val freeRoomPositions = map
        .filterNot(_._2.isDefined)
        .keySet
        .filterNot(_.isHallway)
        // no other amphipod in this room
        .filterNot(p => map
          .filter(_._2.isDefined)
          .filterNot(_._1.isHallway)
          .filter(_._1.x == p.x)
          .exists(e => roomXs.toSeq.sorted.indexOf(p.x) != amphipodIdxMatcher(e._2.get.kind))
        )

      val occupiedHallwayXs = map
        .filter(_._1.isHallway)
        .filter(_._2.isDefined)
        .map(e => e._1.x)
        .toSeq

      freeRoomPositions
        .filter(p => p.x == roomXs.toSeq.sorted.apply(amphipodIdxMatcher(amphipod.kind)))
        .filterNot(fp => {
          val minX = fp.x.min(position.x)
          val maxX = fp.x.max(position.x)
          occupiedHallwayXs.exists(x => minX < x && x < maxX)
        })
        .toSeq
    }

    private def distance(a: Position, b: Position): Int = {
      (a.x - b.x).abs + a.y + b.y
    }

    private def roomXs: Set[Int] = map
      .filterNot(_._1.isHallway)
      .map(_._1.x)
      .toSet

    private def forbiddenHallwayPositions: Seq[Position] = map
      .filter(_._1.isHallway).keys
      .filter(p => roomXs.contains(p.x))
      .toSeq

    private def amphipodIdxMatcher: String => Int = {
      case "Amber" => 0
      case "Bronze" => 1
      case "Copper" => 2
      case "Desert" => 3
    }
  }
}

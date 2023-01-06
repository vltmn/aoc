package io.vltmn.aoc
package y2022

import util.MapUtils.{Coord, CoordUtils}

import scala.annotation.tailrec

class Day17 extends Solution {
  trait Rock {
    def withNewLeftEdge(n: Coord): Rock

    def left: Int

    def right: Int

    def positions: Set[Coord]

    def top: Int

    def leftBottom: Coord
  }

  case class HorizontalRock(leftBottom: Coord) extends Rock {
    private val ds = (0 to 3).map(d => (d, 0)).toSet

    def withNewLeftEdge(n: (Int, Int)): Rock = HorizontalRock(n)

    def right: Int = (leftBottom._1 + 3)

    def positions: Set[(Int, Int)] = ds.map(leftBottom + _)

    def top: Int = leftBottom._2

    def left: Int = leftBottom._1
  }

  case class CrossRock(leftBottom: Coord) extends Rock {
    private val ds = Set((0, 1), (1, 1), (2, 1), (1, 0), (1, 2))

    def withNewLeftEdge(n: (Int, Int)): Rock = CrossRock(n)

    def right: Int = leftBottom._1 + 2

    def positions: Set[(Int, Int)] = ds.map(leftBottom + _)

    def top: Int = leftBottom._2 + 2

    def left: Int = leftBottom._1
  }

  case class AngleRock(leftBottom: Coord) extends Rock {
    private val ds =
      ((0 to 2).map(d => (d, 0)) ++
        (1 to 2).map(d => (2, d))).toSet

    def withNewLeftEdge(n: (Int, Int)): Rock = AngleRock(n)

    def right: Int = leftBottom._1 + 2

    def positions: Set[(Int, Int)] = ds.map(leftBottom + _)

    def top: Int = leftBottom._2 + 2

    def left: Int = leftBottom._1
  }

  case class VerticalRock(leftBottom: Coord) extends Rock {
    private val ds = (0 to 3).map(d => (0, d)).toSet

    def withNewLeftEdge(n: (Int, Int)): Rock = VerticalRock(n)

    def right: Int = leftBottom._1

    def positions: Set[(Int, Int)] = ds.map(leftBottom + _)

    def top: Int = leftBottom._2 + 3

    def left: Int = leftBottom._1
  }

  case class SquareRock(leftBottom: Coord) extends Rock {
    private val ds = Set((0, 0), (0, 1), (1, 0), (1, 1))

    def withNewLeftEdge(n: (Int, Int)): Rock = SquareRock(n)

    def right: Int = leftBottom._1 + 1

    def positions: Set[(Int, Int)] = ds.map(leftBottom + _)

    def top: Int = leftBottom._2 + 1

    def left: Int = leftBottom._1


  }

  type RockGenerator = (Coord) => Rock

  trait Jet {
    def pushRock(rock: Rock): Rock
  }

  case class LeftJet() extends Jet {
    override def pushRock(rock: Rock): Rock = rock.withNewLeftEdge(rock.leftBottom + (-1, 0))
  }

  case class RightJet() extends Jet {
    override def pushRock(rock: Rock): Rock = rock.withNewLeftEdge(rock.leftBottom + (1, 0))
  }

  def optimizeOccupation(occupied: Set[Coord], width: Int): Set[Coord] = {
    // find the top row where all locations are occupied. Remove all occupations under that.
    @tailrec
    def inner(rowIdx: Int): Option[Int] = rowIdx match {
      case 0 => None
      case _ => if ((0 until width).map(x => (x, rowIdx))
        .toSet
        .intersect(occupied).size == width) Some(rowIdx)
      else inner(rowIdx - 1)
    }

    val maxRow = occupied.map(_._2).max
    val foundRow = inner(maxRow)
    foundRow match {
      case None => occupied
      case Some(r) => occupied
        .filter(o => o._2 >= r)
    }
  }

  @tailrec
  private def dropRock(rock: Rock, occupied: Set[Coord], idx: Int, jetPattern: Seq[Jet], width: Int): (Rock, Int) = {
    // effect it by jet, then go down if possible
    val jet = jetPattern(idx)
    val pushedRock = jet.pushRock(rock)
    val pushInvalid = pushedRock.left < 0 || pushedRock.right >= width || occupied.intersect(pushedRock.positions).nonEmpty
    val rockToMove = if (pushInvalid) rock else pushedRock
    val movedRock = rockToMove.withNewLeftEdge(rockToMove.leftBottom._1, rockToMove.leftBottom._2 - 1)
    val newIdx = (idx + 1) % jetPattern.size
    if (movedRock.leftBottom._2 < 0 || occupied.intersect(movedRock.positions).nonEmpty)
      (rockToMove, newIdx)
    else
      dropRock(movedRock, occupied, newIdx, jetPattern, width)
  }

  override def solve(input: String): String = {
    val inp2 = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    val jetPattern = input.split("").map {
      case "<" => LeftJet()
      case ">" => RightJet()
    }
    val width = 7
    val rocks = 2022
    val startDropPos = (2, 3)
    val rockPatternGenerators: Seq[RockGenerator] = Seq(HorizontalRock, CrossRock, AngleRock, VerticalRock, SquareRock)

    def dropRocks(totalRocks: Int, width: Int, startPos: Coord, jetPattern: Seq[Jet], initOccupied: Set[Coord] = Set(), initJetIdx: Int = 0): (Rock, Int) = {
      @tailrec
      def inner(rocksLeft: Int, dropAt: Coord, occupied: Set[Coord], topRock: Rock, jetIdx: Int): (Rock, Int) = rocksLeft match {
        case 0 => (topRock, jetIdx)
        case _ =>
          val rock = rockPatternGenerators((totalRocks - rocksLeft) % rockPatternGenerators.size)(dropAt)
          val (droppedRock, newJetIdx) = dropRock(rock, occupied, jetIdx, jetPattern, width)
          val newTopRock = if (droppedRock.top > topRock.top) droppedRock else topRock
          val newOccupied = if (rocksLeft % 50 == 0)
            optimizeOccupation(occupied.union(droppedRock.positions), width)
          else
            occupied.union(droppedRock.positions)
          val newDropAt = (dropAt._1, newTopRock.top + 4)
          if (rocksLeft % 100 == 0) {
            //println(rocksLeft)
            //printGrid(width, newOccupied)
          }
          inner(rocksLeft - 1, newDropAt, newOccupied, newTopRock, newJetIdx)
      }

      val initRock = HorizontalRock(-1, -1)
      inner(totalRocks, startPos, initOccupied, initRock, initJetIdx)
    }
    val (topRock, _) = dropRocks(rocks, width, startDropPos, jetPattern)
    val p1 = topRock.top + 1
    s"Part1:\n$p1"
  }
}

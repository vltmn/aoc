package io.vltmn.aoc
package y2022

import util.MapUtils.Coord

import scala.annotation.tailrec

class Day9 extends Solution {
  type Direction = Char

  val RIGHT = 'R'
  val LEFT = 'L'
  val UP = 'U'
  val DOWN = 'D'

  type Instruction = (Direction, Int)

  def applyDirection(dir: Direction, pos: Coord): Coord = dir match {
    case LEFT => (pos._1 - 1, pos._2)
    case RIGHT => (pos._1 + 1, pos._2)
    case UP => (pos._1, pos._2 + 1)
    case DOWN => (pos._1, pos._2 - 1)
  }

  def tailFollow(head: Coord, tail: Coord): Coord = {
    val xDiff = head._1 - tail._1
    val yDiff = head._2 - tail._2
    val xMove = Math.abs(yDiff) match {
      case 2 if xDiff == 2 => 1
      case 2 if xDiff == -2 => -1
      case 2 => if (Math.abs(xDiff) < 1) 0 else xDiff
      case _ => if (Math.abs(xDiff) < 1) 0 else if (xDiff < 0) xDiff + 1 else xDiff - 1
    }
    val yMove = Math.abs(xDiff) match {
      case 2 if yDiff == 2 => 1
      case 2 if yDiff == -2 => -1
      case 2 => if (Math.abs(yDiff) < 1) 0 else yDiff
      case _ => if (Math.abs(yDiff) < 1) 0 else if (yDiff < 0) yDiff + 1 else yDiff - 1
    }
    (tail._1 + xMove, tail._2 + yMove)
  }

  def walk(instrs: List[Instruction], knotsCount: Int): Set[Coord] = {
    @tailrec
    def inner(instrs: List[Instruction], knots: Seq[Coord] = (0 until knotsCount).map(_ => (0, 0)), tailPath: Set[Coord] = Set()): Set[Coord] = instrs match {
      case x :: xs =>
        val dir = x._1
        val emptyNewTailPath: Set[Coord] = Set()
        val (newKnots, addedTailpath): (Seq[Coord], Set[Coord]) = (0 until x._2)
          .foldLeft((knots, emptyNewTailPath))((acc, _) => {
            val newHead = applyDirection(dir, acc._1.head)
            val newTail = acc._1.tail.foldLeft(Seq(newHead))((acc, curr) => {
              acc.appended(tailFollow(acc.last, curr))
            }).tail
            val newPath = acc._2.incl(newTail.last)
            (newTail.prepended(newHead), newPath)
          })
        inner(xs, newKnots, tailPath.union(addedTailpath))
      case Seq() => tailPath
    }

    inner(instrs)
  }

  def printPoses(poses: Set[(Int, Int)]): Unit = {
    val minWidth = poses.map(_._1).min
    val minHeight = poses.map(_._2).min
    val maxWidth = poses.map(_._1).max
    val maxHeight = poses.map(_._2).max
    val str = (minHeight to maxHeight).reverse.map(j => (minWidth until maxWidth)
      .map(i => if (i == 0 && j == 0) 's' else if (poses.contains((i, j))) '#' else '.').mkString
    ).mkString("\n")
    println(str)
  }

  override def solve(input: String): String = {
    val instructions: List[Instruction] =
      input.linesIterator
        .map(l => l.split(" "))
        .map(e => (e(0).charAt(0), e(1).toInt))
        .toList

    val p1Poses = walk(instructions, 2)
    val p2Poses = walk(instructions, 10)
    val p1 = p1Poses.size
    val p2 = p2Poses.size
    //printPoses(p1Poses)
    //printPoses(p2Poses)

    s"Part1: $p1\nPart2: $p2"
  }
}

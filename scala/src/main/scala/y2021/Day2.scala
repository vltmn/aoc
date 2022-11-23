package io.vltmn.aoc
package y2021

class Day2 extends Solution {
  def partB(input: String): (Int, Int) = {
    val pos = input.linesIterator
      .map(_.split(" "))
      .map(sa => new Event(sa(0), sa(1).toInt))
      .foldLeft(PositionWithAim(0, 0, 0))((a, b) => b match {
        case Event("forward", value) => PositionWithAim(a.horizontal + value, a.depth + a.aim * value, a.aim)
        case Event("down", value) => PositionWithAim(a.horizontal, a.depth, a.aim + value)
        case Event("up", value) => PositionWithAim(a.horizontal, a.depth, a.aim - value)
      })
    (pos.horizontal, pos.depth)
  }

  def partA(input: String): (Int, Int) = {
    val pos = input.linesIterator
      .map(_.split(" "))
      .map(sa => new Event(sa(0), sa(1).toInt))
      .foldLeft(Position(0, 0))((a, b) => b match {
        case Event("forward", value) => Position(a.horizontal + value, a.depth)
        case Event("down", value) => Position(a.horizontal, a.depth + value)
        case Event("up", value) => Position(a.horizontal, a.depth - value)
      })
    (pos.horizontal, pos.depth)
  }

  override def solve(input: String): String = {
    val (paHorizontal, paDepth) = partA(input)
    val (pbHorizontal, pbDepth) = partB(input)
    s"Part A: ${paHorizontal * paDepth}\nPart B: ${pbHorizontal * pbDepth}"
  }

  case class Event(action: String, value: Int)

  case class Position(horizontal: Int, depth: Int)

  case class PositionWithAim(horizontal: Int, depth: Int, aim: Int)
}

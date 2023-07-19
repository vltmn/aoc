package io.vltmn.aoc
package y2022

import util.{InputReader, Position}

import org.scalatest.funsuite.AnyFunSuiteLike

class Day22Test extends AnyFunSuiteLike {

  val day = new Day22()
  test("P1 applies directions") {
    val input = "        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.\n\n10R5L5R10L4R5L5"
    val splitted = input.split("\n\n")
    val (map, initialPos) = day.parseMap(splitted.head)
    val directions = day.parseDirections(splitted.last)
    val expectedFinalPos = (Position(7, 5), day.Right())

    // when
    val finalPos = day.applyDirections(map, initialPos, directions, day.mkPart1Wrap(map))

    // then
    assertResult(expectedFinalPos)(finalPos)
  }

  test("P1 moves correctly") {
    val (map, _, _) = getMapFromData()

    val initialPos = (Position(40, 106), day.Up())
    val dirs = Seq(day.Move(9))
    val expectedFinalPos = (Position(40, 197), day.Up())

    val finalPos = day.applyDirections(map, initialPos, dirs, day.mkPart1Wrap(map))

    assertResult(expectedFinalPos)(finalPos)
  }

  test("P2 wraps around same direction and axis correctly") {
    val (map, _, _) = getMapFromData()

    val initialPos = (Position(138, 0), day.Up())
    val dirs = Seq(day.Move(1), day.LeftTurn(), day.LeftTurn(), day.Move(1))
    val expectedFinalPos = (initialPos._1, day.Down())

    val finalPos = day.applyDirections(map, initialPos, dirs, day.mkPart2Wrap(map))

    assertResult(expectedFinalPos)(finalPos)
  }

  test("P2 moves up from 2 correctly") {
    val (map, _, _) = getMapFromData()

    val initialPos = (Position(89, 0), day.Up())
    val dirs = Seq(day.Move(1))
    val expectedFinalPos = (Position(89, 0), day.Up())

    val finalPos = day.applyDirections(map, initialPos, dirs, day.mkPart2Wrap(map))

    assertResult(expectedFinalPos)(finalPos)
  }


  private def getMapFromData() = {
    val reader = new InputReader(2022, 22)
    val data = reader.read()
    val splitted = data.split("\n\n")
    val (map, initialPos) = day.parseMap(splitted.head)
    val dirs = day.parseDirections(splitted.last)
    (map, initialPos, dirs)
  }
}

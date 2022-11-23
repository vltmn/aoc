package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {
  val day = new Day9
  val demoInput = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

  test("Part1 works correctly") {
    assertResult(15)(day.part1(demoInput))
  }

  test("Part2 works correctly") {
    assertResult(1134)(day.part2(demoInput))
  }

  test("FindBasinIdx test 1") {
    val (data, width) = day.parseInput(demoInput)
    assertResult(List(0, 1, 10))(day.findBasinIdxs(1, data, width))
  }

  test("FindBasinIdx test 2") {
    val (data, width) = day.parseInput(demoInput)
    assertResult(List(5, 6, 7, 8, 9, 16, 18, 19, 29))(day.findBasinIdxs(9, data, width))
  }

}

package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {
  val day = new Day17
  val demoInput = "target area: x=20..30, y=-10..-5"

  test("Part1 works correctly") {
    assertResult(45)(day.part1(demoInput))
  }

  test("Part2 works correctly") {
    assertResult(112)(day.part2(demoInput))
  }
}

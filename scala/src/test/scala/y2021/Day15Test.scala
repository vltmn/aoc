package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {
  val day = new Day15
  val demoInput = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"

  test("Part 1 works correctly") {
    assertResult(40)(day.part1(demoInput))
  }

  test("Part 2 works correctly") {
    assertResult(315)(day.part2(demoInput))
  }

}

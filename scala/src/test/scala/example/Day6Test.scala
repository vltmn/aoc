package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {
  val day6 = new Day6
  val demoInput = "3,4,3,1,2"
  test("Part 1 works with demo input") {
    assertResult(5934)(day6.part1(demoInput))
  }

  test("Part2 works with demo input") {
    assertResult(26984457539L)(day6.part2(demoInput))
  }

}

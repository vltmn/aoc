package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {
  val day = new Day7
  val demoInput = "16,1,2,0,4,2,7,1,2,14"

  test("Part 1 works") {
    assertResult(37)(day.part1(demoInput))
  }

  test("Part2 works") {
    assertResult(168)(day.part2(demoInput))
  }

  test("Calc increasing cost works for 16") {
    assertResult(66)(day.calcIncreasingCost(List(16), 5))
  }

  test("Calc increasing cost works for 0") {
    assertResult(15)(day.calcIncreasingCost(List(0), 5))
  }

  test("Calc increasing cost works for 4") {
    assertResult(1)(day.calcIncreasingCost(List(4), 5))
  }
  test("Calc increasing cost works for 7") {
    assertResult(3)(day.calcIncreasingCost(List(7), 5))
  }

}

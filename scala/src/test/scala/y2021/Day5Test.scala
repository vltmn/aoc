package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {
  val day5 = new Day5
  val demoInput = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
  test("Part 1 works with demo input") {
    assertResult(5)(day5.part1(demoInput))
  }

  test("Part2 works with demo input") {
    assertResult(12)(day5.part2(demoInput))
  }

}

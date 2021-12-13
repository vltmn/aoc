package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {
  val day = new Day13
  val demoInput = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"

  test("Part 1 works correctly") {
    assertResult(17)(day.part1(demoInput))
  }

  test("Part 2 works correctly") {
    println(day.part2(demoInput))
  }

}

package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {
  val day = new Day21
  val demoInput = "Player 1 starting position: 4\nPlayer 2 starting position: 8"

  test("Part1 works") {
    assertResult(739785)(day.part1(demoInput))
  }

  test("Part2 works") {
    assertResult(444356092776315L)(day.part2(demoInput))
  }

}

package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {
  val day3 = new Day3()
  val demoInput = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

  test("Part 1 solvess demo puzzle") {
    assertResult((22, 9))(day3.part1(demoInput))
  }

  test("Part 2 solves demo puzzle") {
    assertResult((23, 10))(day3.part2(demoInput))
  }

}

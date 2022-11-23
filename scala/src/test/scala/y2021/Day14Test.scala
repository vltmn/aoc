package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {
  val day = new Day14
  val demoInput = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"

  test("Part1 works correctly") {
    assertResult(1588)(day.part1(demoInput))
  }

  test("Part2 works correctly") {
    assertResult(2188189693529L)(day.part2(demoInput))
  }

}

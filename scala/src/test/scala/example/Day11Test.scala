package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {
  val day = new Day11
  val demoInput = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

  test("Part1 works correctly") {
    assertResult(1656)(day.part1(demoInput))
  }

  test("Part2 works correctly") {
    assertResult(195)(day.part2(demoInput))
  }

  test("Do step works correctly") {
    val (board, width) = day.parseInput("11111\n19991\n19191\n19991\n11111")
    val newBoard = day.doStep(board, width)
    assertResult(List(3, 4, 5, 4, 3, 4, 0, 0, 0, 4, 5, 0, 0, 0, 5, 4, 0, 0, 0, 4, 3, 4, 5, 4, 3))(newBoard)
    assertResult(9)(newBoard.count(_ == 0))
  }

}

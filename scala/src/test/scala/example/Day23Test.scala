package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends AnyFunSuite {
  val day = new Day23
  val demoInput = "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########"

  test("Part1 works correctly") {
    assertResult(12521)(day.part1(demoInput))
  }

  test("Get neighbors") {

    //var state = day.parseInput("#############\n#...B.......#\n###B#C#.#D###\n  #A#D#C#A#\n  #########")
    //var neighbors = state.getNeighbors
    assertResult(1)(day.parseInput("#############\n#...B.......#\n###B#.#C#D###\n  #A#D#C#A#\n  #########").getNeighbors.size)
    assertResult(3)(day.parseInput("#############\n#.....D.....#\n###.#B#C#D###\n  #A#B#C#A#\n  #########").getNeighbors.size)
    assertResult(1)(day.parseInput("#############\n#.....D.D.A.#\n###.#B#C#.###\n  #A#B#C#.#\n  #########").getNeighbors.size)
    assertResult(7)(day.parseInput("#############\n#.....D.....#\n###B#.#C#D###\n  #A#B#C#A#\n  #########").getNeighbors.size)
    //val state = day.parseInput("#############\n#.........A.#\n###.#B#C#D###\n  #A#B#C#D#\n  #########")
  }

}

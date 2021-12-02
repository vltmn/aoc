package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {
  val day2 = new Day2()
  val demoInput = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
  test("Demo input part A") {
    assertResult((15, 10))(day2.partA(demoInput))
  }

  test("Demo input part B") {
    assertResult((15, 60))(day2.partB(demoInput))
  }

}

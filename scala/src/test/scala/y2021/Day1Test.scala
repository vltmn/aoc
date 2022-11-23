package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {
  val day1 = new Day1()
  val demoInput = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263";
  test("Demo input part A") {
    assert(day1.partA(demoInput) == 7)
  }
  test("Demo input part B") {
    assert(day1.partB(demoInput) == 5)
  }

}

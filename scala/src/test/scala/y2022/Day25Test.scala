package io.vltmn.aoc
package y2022

import org.scalatest.funsuite.AnyFunSuiteLike

class Day25Test extends AnyFunSuiteLike {
  val day = new Day25()
  test("Part1 converts from snafu to base10") {
    val snafu = "2=0="
    val expected = 198

    val actual = day.fromSnafu(snafu)
    assertResult(expected)(actual)
  }

  test("Part1 converts to snafu from base10") {
    val value = 198
    val expected = "2=0="

    val actual = day.toSnafu(value)
    assertResult(expected)(actual)
  }

}

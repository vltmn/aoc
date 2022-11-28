package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {
  val day = new Day25

  def runStepOnInput(input: String): String = {
    val (height, width, cucumbers) = day.parseInput(input)
    val done = day.step(cucumbers, width, height)
    day.cucumberMapToString(done, width, height)
  }
  test("multiMoveTest") {
    val input = "..........\n.>v....v..\n.......>..\n.........."
    val expected = "..........\n.>........\n..v....v>.\n.........."
    val output = runStepOnInput(input)
    assert(output == expected)
  }

  test("borderMove") {
    val input = "......>\n..v.v..\n..>v...\n>......\n..>....\nv......\n......."
    val expected = ">......\n..v....\n..>.v..\n.>.v...\n...>...\n.......\nv......"
    val output = runStepOnInput(input)
    assert(output == expected)
  }

  test("stopTest") {
    val input = "..>>v>vv..\n..v.>>vv..\n..>>v>>vv.\n..>>>>>vv.\nv......>vv\nv>v....>>v\nvvv.....>>\n>vv......>\n.>v.vv.v.."
    val output = runStepOnInput(input)

    assert(output == input)
  }

}

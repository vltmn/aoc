package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {
  val day = new Day10
  val demoInput = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

  test("Part1 works correctly") {
    assertResult(26397)(day.part1(demoInput))
  }

  test("Part2 works correctly") {
    assertResult(288957)(day.part2(demoInput))
  }

  test("Matcher works correctly incomplete") {
    val data = "[(()[<>])]({[<{<<[]>>(".toCharArray.toList
    assertResult((None, List('(', '{', '[', '<', '{', '(')))(day.matcher(data, List()))
  }

  test("Matcher works correctly corrupted") {
    val data = "{([(<{}[<>[]}>{[]{[(<()>".toCharArray.toList
    assertResult((Some('}'), List('{', '(', '[', '(', '<', '[')))(day.matcher(data, List()))
  }

}

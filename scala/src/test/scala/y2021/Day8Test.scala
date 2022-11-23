package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends AnyFunSuite {
  val day = new Day8
  val demoInput = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

  test("Part 1 works on demo input") {
    assertResult(26)(day.part1(demoInput))
  }

  test("Part 2 works on demo input") {
    assertResult(61229)(day.part2(demoInput))
  }

  test("BuildPossibilities correct") {
    assertResult(1680)(day.buildWirings("abcdefg").size)
  }

  test("mapWiring is correct") {
    assertResult(Some(5))(day.mapWiring("cdfeb", "deafgbc", "abcdefg"))
  }

  test("findWiring is correct") {
    assertResult("deafgbc")(day.findWiring(
      List("acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"),
      day.buildWirings("abcdefg"),
      "abcdefg"
    ))
  }
}

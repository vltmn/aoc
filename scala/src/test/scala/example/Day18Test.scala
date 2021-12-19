package io.vltmn.aoc2021
package example

import example.Day18.{Left, Pair, Right, Value}

import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {
  val day = new Day18
  val demoInput = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

  test("PairSplitter works") {
    assertResult(("1", "2"))(day.pairSplitter("1,2"))
    assertResult(("[4,3]", "2"))(day.pairSplitter("[4,3],2"))
    assertResult(("[[[1,2],[3,4]],[[5,6],[7,8]]]", "9"))(day.pairSplitter("[[[1,2],[3,4]],[[5,6],[7,8]]],9"))
  }

  test("ParsePair works") {
    assertResult(Pair(Value(2), Value(1)))(day.parsePair("[2,1]"))
    assertResult(Pair(
      Pair(
        Pair(
          Pair(Value(1), Value(2)),
          Pair(Value(3), Value(4))
        ),
        Pair(
          Pair(Value(5), Value(6)),
          Pair(Value(7), Value(8))
        ),
      ), Value(9)))(day.parsePair("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"))
  }

  test("Indices work") {
    assertResult(Seq(Seq(new Left), Seq(new Right)))(Pair(Value(1), Value(2)).indices())
    assertResult(Seq(Seq(new Left, new Left), Seq(new Left, new Right), Seq(new Right)))(Pair(Pair(Value(4), Value(3)), Value(2)).indices())
  }

  test("Values work") {
    assertResult(Seq(4, 3, 2).map(Value))(Pair(Pair(Value(4), Value(3)), Value(2)).values())
  }

  test("Replace work") {
    val start = Pair(Pair(Value(4), Value(3)), Value(2))
    assertResult(Some(Pair(Pair(Value(4), Value(5)), Value(2))))(start.replaceAt(Seq(new Left, new Right), Value(5)))
  }

  test("Explode works") {
    var start = day.parsePair("[[[[[9,8],1],2],3],4]")
    var expected = Pair(Pair(Pair(Pair(Value(0), Value(9)), Value(2)), Value(3)), Value(4))
    assertResult(expected)(start.explode(Seq(new Left, new Left, new Left, new Left)))
    start = day.parsePair("[[6,[5,[4,[3,2]]]],1]")
    expected = day.parsePair("[[6,[5,[7,0]]],3]")
    assertResult(expected)(start.explode(Seq(new Left, new Right, new Right, new Right)))
  }

  test("Part1 works") {
    assertResult(4140)(day.part1(demoInput))
  }

  test("Part2 works") {
    assertResult(3993)(day.part2(demoInput))
  }
}

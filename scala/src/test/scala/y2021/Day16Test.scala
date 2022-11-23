package io.vltmn.aoc
package y2021

import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {
  val day = new Day16

  test("Part 1 works input 1") {
    val demoInput = "8A004A801A8002F478"
    assertResult(16)(day.part1(demoInput))
  }

  test("Part 1 works input 2") {
    val demoInput = "620080001611562C8802118E34"
    assertResult(12)(day.part1(demoInput))
  }

  test("Part 1 works input 3") {
    val demoInput = "C0015000016115A2E0802F182340"
    assertResult(23)(day.part1(demoInput))
  }

  test("Part 1 works input 4") {
    val demoInput = "A0016C880162017C3686B18A3D4780"
    assertResult(31)(day.part1(demoInput))
  }

  test("Part 1 works input 5") {
    val demoInput = "D2FE28"
    assertResult(6)(day.part1(demoInput))
  }

  test("Part 2 works input 1") {
    val demoInput = "C200B40A82"
    assertResult(3)(day.part2(demoInput))
  }

  test("Part 2 works input 2") {
    val demoInput = "04005AC33890"
    assertResult(54)(day.part2(demoInput))
  }

  test("Part 2 works input 3") {
    val demoInput = "880086C3E88112"
    assertResult(7)(day.part2(demoInput))
  }

  test("Part 2 works input 4") {
    val demoInput = "CE00C43D881120"
    assertResult(9)(day.part2(demoInput))
  }

  test("Part 2 works input 5") {
    val demoInput = "D8005AC2A8F0"
    assertResult(1)(day.part2(demoInput))
  }

  test("Part 2 works input 6") {
    val demoInput = "F600BC2D8F"
    assertResult(0)(day.part2(demoInput))
  }

  test("Part 2 works input 7") {
    val demoInput = "9C005AC2F8F0"
    assertResult(0)(day.part2(demoInput))
  }

  test("Part 2 works input 8") {
    val demoInput = "9C0141080250320F1802104A08"
    assertResult(1)(day.part2(demoInput))
  }
}

package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {
  val day = new Day12

  test("Part 1 works example 1") {
    val input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
    assertResult(10)(day.part1(input))
  }

  test("Part 1 works example 2") {
    val input = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"
    assertResult(19)(day.part1(input))
  }

  test("Part 1 works example 3") {
    val input = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"
    assertResult(226)(day.part1(input))
  }

  test("Part 2 works example 1") {
    val input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
    assertResult(36)(day.part2(input))
  }

  test("Part 2 works example 2") {
    val input = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"
    assertResult(103)(day.part2(input))
  }

  test("Part 2 works example 3") {
    val input = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"
    assertResult(3509)(day.part2(input))
  }
}

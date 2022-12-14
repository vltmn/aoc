package io.vltmn.aoc
package y2022


import io.vltmn.aoc.util.InputReader
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  val d = new Day13
  test("testParse") {
    val reader = new InputReader(2022, 13)
    val data = reader.read()
    val parsed = data.split("\n\n")
      .map(_.split("\n"))
      .map(_
        .map(_.drop(1).dropRight(1))
        .map(d.parse))
    val str = parsed.map(p => p.map(_.toString).mkString("\n"))
      .mkString("\n\n")

    assert(data == str)
  }

  test("testParse2") {
    val reader = new InputReader(2022, 13)
    val data = reader.read()
    val parsed = data
      .split("\n\n")
      .map(_.split("\n"))
      .map(_
        .map(d.parse2))
    val str = parsed.map(p => p.map(_.toString).mkString("\n"))
      .mkString("\n\n")

    assert(data == str)
  }

  test("testParse2 small") {
    val reader = new InputReader(2022, 13)
    val data = reader.read().split("\n").head
    val parsed = data
      .split("\n\n")
      .map(_.split("\n"))
      .map(_
        .map(d.parse2))
    val str = parsed.map(p => p.map(_.toString).mkString("\n"))
      .mkString("\n\n")

    assert(str == data)
  }

}

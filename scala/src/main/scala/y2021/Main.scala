package io.vltmn.aoc
package y2021

import util.InputReader

object Main extends App {
  val solution: Solution = new Day1()
  val day: Int = solution.getClass.getSimpleName.substring(3).toInt
  val year = 2021
  val reader = new InputReader(year, day)
  val data = reader.read()
  val output = solution.solve(data)

  println(s"Output was:\n${output}")
}

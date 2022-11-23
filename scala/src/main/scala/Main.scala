package io.vltmn.aoc

import util.InputReader

object Main extends App {
  val solution: Solution = new y2021.Day1()
  val day: Int = solution.getClass.getSimpleName.substring(3).toInt
  val year = solution.getClass.getPackageName.split("\\.").last.substring(1).toInt
  val reader = new InputReader(year, day)
  val data = reader.read()
  val output = solution.solve(data)

  println(s"Output was:\n${output}")
}

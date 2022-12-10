package io.vltmn.aoc

import util.InputReader

object Main extends App {
  val solution: Solution = new y2022.Day10()
  val day: Int = solution.getClass.getSimpleName.substring(3).toInt
  val year = solution.getClass.getPackageName.split("\\.").last.substring(1).toInt
  val reader = new InputReader(year, day)
  val data = reader.read()
  val pre = System.nanoTime();
  val output = solution.solve(data)
  val execTime = System.nanoTime() - pre
  println(s"Output was:\n${output}")
  val execTimeFormatted = execTime.toDouble / Math.pow(10, 9)
  println(s"Execution time: $execTimeFormatted s")
}

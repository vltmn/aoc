package io.vltmn.aoc2021
package example

import scala.io.Source

class InputReader(filename: String) {
  def read(): String = {
    val src = Source.fromFile(getClass.getResource("/" + filename).getFile)
    val data = src.mkString
    src.close()
    data
  }
}

object Main extends App {
  val solution: Solution = new Day10()
  val reader = new InputReader(solution.getClass.getSimpleName.toLowerCase())
  val data = reader.read()
  val output = solution.solve(data)

  println(s"Output was:\n${output}")
}

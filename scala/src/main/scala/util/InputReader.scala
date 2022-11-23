package io.vltmn.aoc
package util

import scala.io.Source

class InputReader(year: Int, day: Int) {
  def read(): String = {
    val src = Source.fromFile(getClass.getResource(s"/y$year/day$day").getFile)
    val data = src.mkString
    src.close()
    data
  }
}

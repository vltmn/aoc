package io.vltmn.aoc2021
package example

class Day0 extends Solution {
  def supportsTLS(input: String): Boolean = {
    val splitted = input.split(Array('[', ']'))
    splitted.zipWithIndex.filter(_._2 % 2 == 0).exists(t => isAbba(t._1)) &&
      splitted.zipWithIndex.filter(_._2 % 2 == 1).forall(t => !isAbba(t._1))
  }

  val isAbba: String => Boolean = (input: String) => {
    val inner = (s: String) => s(0) == s(3) && s(1) == s(2) && s(0) != s(1) && s.length == 4
    inner(input.take(4)) || (input.length > 4 && isAbba(input.drop(1)))
  }

  override def solve(input: String): String = {
    input.lines().filter(supportsTLS).count().toString
  }
}

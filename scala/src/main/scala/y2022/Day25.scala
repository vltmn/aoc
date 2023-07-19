package io.vltmn.aoc
package y2022

class Day25 extends Solution {
  type Num = Long

  def fromSnafuDigit(digit: Char): Num = digit match {
    case '=' => -2
    case '-' => -1
    case '0' => 0
    case '1' => 1
    case '2' => 2
  }
  def fromSnafu(snafu: String): Num = snafu.toCharArray.reverse.zipWithIndex
    .map(e => fromSnafuDigit(e._1) * math.pow(5, e._2).toLong).sum

  def toSnafu(value: Num): String = value match {
    case 0 => ""
    case _ => value % 5 match {
      case 0 => toSnafu(value / 5) + "0"
      case 1 => toSnafu(value / 5) + "1"
      case 2 => toSnafu(value / 5) + "2"
      case 3 => toSnafu((value + 2)/5) + "="
      case 4 => toSnafu((value + 3) / 5) + "-"
    }
  }
  override def solve(input: String): String = {
    val realNums = input.split("\n").map(l => (l, fromSnafu(l)))
    val p1 = toSnafu(realNums.map(_._2).sum)
    s"P1: $p1"
  }
}

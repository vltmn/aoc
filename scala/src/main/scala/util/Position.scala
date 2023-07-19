package io.vltmn.aoc
package util

case class Position(x: Int, y: Int) extends Comparable[Position] {
  override def compareTo(o: Position): Int = Ordering.Tuple2[Int, Int].compare((y, x), (o.y, o.x))

  def +(other: Position): Position = Position(x + other.x, y + other.y)
}

object Position {
  def apply(pair: (Int, Int)): Position = new Position(pair._1, pair._2)
}
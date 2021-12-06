package io.vltmn.aoc2021
package example

class Day6 extends Solution {
  val simDays = 80

  def parseInput(in: String): Seq[Int] = in.split(",").map(_.toInt)

  def runSimulation(world: Seq[Int], days: Int): Map[Int, Long] = {
    val grouper: Seq[Int] => Map[Int, Long] =  _
      .groupBy(identity)
      .map(d => (d._1, d._2.size))
    (0 until days)
      .foldLeft(grouper(world))((acc, _) => {

        val toAdd: Long = acc.getOrElse(0, 0L)
        val newSixes: Long = toAdd + acc.getOrElse(7, 0L)
        val changed: Map[Int, Long] = acc
          .filter(d => d._2 > 0)
          .map(d => d._1 match {
            case 0 => (6, d._2)
            case v => (v - 1, d._2)
          })

        changed + (8 -> toAdd) + (6 -> newSixes)
      })
  }

  def countTotalLanterns(data: Map[Int, Long]): Long = data.values.sum

  def part1(input: String): Long = {
    val world = parseInput(input)
    val endData = runSimulation(world, 80)
    countTotalLanterns(endData)
  }

  def part2(input: String): Long = {
    val world = parseInput(input)
    val endData = runSimulation(world, 256)
    countTotalLanterns(endData)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: ${p1}\nPart2: ${p2}"
  }
}

package io.vltmn.aoc
package y2022

import util.MapUtils.{Coord, CoordUtils}


class Day15 extends Solution {
  private implicit class RangeUtils(r: Range) {
    def merge(o: Range): Range = r.start.min(o.start) to r.end.max(o.end)
    def overlaps(o: Range): Boolean = if (o.start < r.start) r.start - o.end <= 1 else o.start - r.end <= 1
  }

  def makeRanges(sensorBeaconDists: Seq[(Coord, Int)], idx: Int): Seq[Range] = sensorBeaconDists
    .foldLeft(Seq[Range]())((acc, curr) => {
      val rangeSize = curr._2 - (curr._1._2 - idx).abs
      if (rangeSize < 0)
        acc
      else {
        val range: Range = curr._1._1 - rangeSize to curr._1._1 + rangeSize
        val (overlapping, others) = acc.partition(_ overlaps range)
        val merged = overlapping.foldLeft(range)(_ merge _)
        merged +: others
      }
    })

  override def solve(input: String): String = {
    val pattern = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
    val parsed: Seq[(Coord, Coord)] = input.linesIterator
      .map(pattern.findFirstMatchIn(_).get)
      .map(m => (1 to 4).map(m.group).map(_.toInt))
      .map(s => ((s(0), s(1)), (s(2), s(3))))
      .toSeq

    val sensorBeaconDistances = parsed
      .map(e => (e._1, e._1.manhattanDistance(e._2)))

    val beacons = parsed.map(_._2).toSet
    val rowToSearch = 2000000
    val ranges = makeRanges(sensorBeaconDistances, rowToSearch)
    val beaconsOnRow = beacons.count(_._2 == rowToSearch)
    val p1 = ranges.map(_.size).sum - beaconsOnRow

    val p2Max = 4000000
    val p2Poses = (0 to p2Max)
      .map(y => (y, makeRanges(sensorBeaconDistances, y)))
      .filter(_._2.length > 1)
      .map(e => (e._2.head.start.max(e._2.last.start) - 1, e._1))
      .filterNot(beacons.contains)

    val p2Pos = p2Poses.head
    val p2 = p2Pos._1 * 4000000L + p2Pos._2

    s"Part1:\n$p1\nPart2:\n$p2"
  }
}

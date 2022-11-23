package io.vltmn.aoc
package y2021

class Day12 extends Solution {
  type Connections = Map[String, Seq[String]]

  type Path = Seq[String]

  type NextFilter = (String, Seq[String]) => Boolean

  def parseInput(input: String): Connections = {
    input
      .linesIterator
      .map(_.split("-"))
      .map(s => (s(0), s(1)))
      .flatMap(tpl => List((tpl._1, tpl._2), (tpl._2, tpl._1)))
      .foldLeft(Map[String, Seq[String]]())((acc, curr) =>
        acc + (curr._1 -> acc.getOrElse(curr._1, List()).appended(curr._2).distinct)
      )
  }

  def findPaths(connections: Connections, nextFilter: NextFilter): Seq[Path] = {

    def inner(connections: Connections, history: Seq[String]): Seq[Path] = {
      val current = history.last
      val nexts = connections.get(current)
        .orElse(Some(List())).get
        .filter(nextFilter(_, history))
      nexts.flatMap {
        case "end" => List(history.appended("end"))
        case c => inner(connections, history.appended(c))
      }
    }
    inner(connections, List("start"))
  }

  def part1(input: String): Int = {
    val data = parseInput(input)
    val nextFilter: NextFilter = (s, history) =>
      !(s.toLowerCase == s && history.contains(s))

    val paths = findPaths(data, nextFilter)

    paths.size
  }

  def part2(input: String): Int = {
    val data = parseInput(input)
    val nextFilter: NextFilter = (s, history) => {
      val lowerCases = history.filter(d => d != "end" && d != "start" && d.toLowerCase == d)
      val ended = history.contains("end")
      val hasDoubleSmallCave = lowerCases.size != lowerCases.distinct.size
      val myCaveCount = lowerCases.count(_ == s)
      if (s == "start")
        false
      else if (hasDoubleSmallCave)
        !(s.toLowerCase == s && myCaveCount > 0)
      else
        !ended

    }
    val paths = findPaths(data, nextFilter)
   // val paths = findPaths(data)

    paths.size
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

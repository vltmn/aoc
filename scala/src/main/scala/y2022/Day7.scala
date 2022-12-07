package io.vltmn.aoc
package y2022

class Day7 extends Solution {
  type File = (Int, String)

  override def solve(input: String): String = {
    def getDirMap(rows: Seq[String]): Map[Seq[String], Seq[File]] = {
      def inner(rows: Seq[String], currPath: Seq[String]): Seq[(Seq[String], Option[File])] = rows match {
        case x :: xs => x match {
          case "$ cd /" => inner(xs, Seq())
          case "$ cd .." => inner(xs, currPath.dropRight(1))
          case s if s.startsWith("$ cd ") => inner(xs, currPath.appended(s.drop(5)))
          case "$ ls" => inner(xs, currPath)
          case s if s.startsWith("dir ") => inner(xs, currPath).appended((currPath, None))
          case s =>
            val splitted = s.split(" ")
            val size = splitted.head.toInt
            val name = splitted.last
            inner(xs, currPath).appended(currPath, Some(size, name))
        }
        case _ => Seq()
      }
      inner(rows, Seq())
        .groupMapReduce(_._1)(e => Seq(e._2).flatten)(_.appendedAll(_))
    }

    val d = input.linesIterator.toSeq
    val dirs = getDirMap(d)
    val dirSizes = dirs
      .map(d => (d._1, dirs.filter(_._1.startsWith(d._1)).flatMap(_._2).keys.sum))
    val p1 = dirSizes.filter(e => e._2 <= 100000).values.sum
    val spaceAvailable = 70000000
    val unusedNeeded = 30000000
    val currentUsed = dirSizes(Seq())
    val needsToBeDeleted = unusedNeeded - (spaceAvailable - currentUsed)
    val p2 = dirSizes.filter(_._2 >= needsToBeDeleted).values.min
    s"Part1: $p1\nPart2: $p2"
  }
}

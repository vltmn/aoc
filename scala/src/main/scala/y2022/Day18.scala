package io.vltmn.aoc
package y2022

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

class Day18 extends Solution {
  case class Cube(x: Int, y: Int, z: Int)

  override def solve(input: String): String = {
    val parsed = input
      .linesIterator
      .map(_.split(",").map(_.toInt))
      .map(r => Cube(r(0), r(1), r(2)))
      .toSeq

    def cubesToMap(cubes: Seq[Cube], keyMap: Cube => Int): SortedMap[Int, Set[Cube]] =
      TreeMap.from(cubes.groupMapReduce(keyMap)(Set(_))(_.union(_)))

    val xMap = cubesToMap(parsed, _.x)
    val yMap = cubesToMap(parsed, _.y)
    val zMap = cubesToMap(parsed, _.z)

    val deltaCubes = Seq((0, 0, 1), (0, 0, -1), (0, 1, 0), (0, -1, 0), (1, 0, 0), (-1, 0, 0))
      .map(e => Cube(e._1, e._2, e._3))

    def buildOneDiffSet(map: SortedMap[Int, Set[Cube]], v: Int): Set[Cube] =
      map.getOrElse(v - 1, Set()) union map.getOrElse(v + 1, Set())

    val coveredSides = parsed
      .map(c => {
        val y0 = yMap(c.y)
        val x0 = xMap(c.x)
        val z0 = zMap(c.z)

        val xy0 = x0 intersect y0
        val yz0 = y0 intersect z0
        val xz0 = x0 intersect z0

        val x1 = buildOneDiffSet(xMap, c.x)
        val y1 = buildOneDiffSet(yMap, c.y)
        val z1 = buildOneDiffSet(zMap, c.z)
        val covered = (xy0 intersect z1) union
          (yz0 intersect x1) union
          (xz0 intersect y1)
        (c, covered)
      })
    val exposedSides = coveredSides.map(e => {
      val deltas = deltaCubes.map(dc => Cube(e._1.x + dc.x, e._1.y + dc.y, e._1.z + dc.z))
      (e._1, deltas.filterNot(e._2.contains))
    })
    val emptyCloseCubes = exposedSides.map(_._2.toSet).reduce(_ union _)

    val cubesSet = parsed.toSet
    val partitions = emptyCloseCubes.foldLeft(Set[Set[Cube]]())((acc, cube) => {
      // find all clusters containing cube
      // merge them if existing, otherwise add new cluster
      val closeCubes = deltaCubes.map(dc => Cube(cube.x + dc.x, cube.y + dc.y, cube.z + dc.z))
        .filterNot(cubesSet.contains).toSet
      val containing = acc.filter(cs => cs.contains(cube) || cs.intersect(closeCubes).nonEmpty)

      if (containing.isEmpty) {
        acc union Set(closeCubes union Set(cube))
      } else {
        val merged = containing.reduce(_ union _) union closeCubes union Set(cube)
        (acc diff containing) union Set(merged)
      }
    })
    val internalPartitions = partitions
      .filter(_.size < partitions.map(_.size).max)
      .reduce(_ union _)

    val p1 = exposedSides.map(_._2.size).sum
    val p2 = exposedSides
      .map(_._2)
      .map(_.filterNot(internalPartitions.contains))
      .map(_.size)
      .sum
    s"Part1:\n$p1\nPart2:\n$p2"
  }
}

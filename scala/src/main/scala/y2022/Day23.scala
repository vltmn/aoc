package io.vltmn.aoc
package y2022

import scala.annotation.tailrec
import scala.collection.SortedSet

class Day23 extends Solution {
  type Num = Int

  case class Position(x: Num, y: Num) extends Comparable[Position] {
    override def compareTo(o: Position): Num = Ordering.Tuple2[Num, Num].compare((y, x), (o.y, o.x))

    def +(other: Position): Position = Position(x + other.x, y + other.y)
  }

  trait BoardCell

  object BoardCell {
    private case class Elf() extends BoardCell

    private case class Open() extends BoardCell

    val ELF: BoardCell = Elf()
    val OPEN: BoardCell = Open()
  }

  trait Direction {
    def deltas(): SortedSet[Position] = {
      val ds = Range.inclusive(-1, 1)
        .map(v => (value() match {
          case 0 => -1
          case 1 => 1
          case 2 => 1
          case 3 => -1
        }, v))
        .map(v => if (value() % 2 == 0) v.swap else v)
        .map(v => Position(v._1, v._2))
      SortedSet.from(ds)
    }

    def value(): Num

    def delta(): Position = deltas().tail.head
  }

  object Direction {
    private case class North() extends Direction {
      override def value(): Num = 0
    }

    private case class South() extends Direction {
      override def value(): Num = 2
    }

    private case class East() extends Direction {
      override def value(): Num = 1
    }

    private case class West() extends Direction {
      override def value(): Num = 3
    }

    val N: Direction = North()
    val S: Direction = South()
    val E: Direction = East()
    val W: Direction = West()
  }

  def parseMap(str: String): Set[Position] = str.split("\n")
    .map(_.toCharArray)
    .zipWithIndex
    .flatMap(e => e._1.map {
      case '.' => BoardCell.OPEN
      case '#' => BoardCell.ELF
    }.zipWithIndex.map(v => (Position(v._2, e._2), v._1))
    ).filter(_._2 == BoardCell.ELF).map(_._1).toSet

  def proposeNewPose(pos: Position, elfPoses: Set[Position], directionOrder: Seq[Direction]): Option[Position] = {
    directionOrder.find(d => d.deltas().map(_ + pos).intersect(elfPoses).isEmpty)
      .map(d => d.delta() + pos)
  }

  def simulate(elfPoses: Set[Position], directionOrder: Seq[Direction], allDeltas: Set[Position]): Set[Position] = {
    val newPoses = elfPoses
      // map to tuple with all deltas
      .map(ep => (ep, allDeltas.map(_ + ep)))
      .map(v =>
        if (v._2.intersect(elfPoses).isEmpty)
          (v._1, v._1)
        else
          (v._1, proposeNewPose(v._1, elfPoses, directionOrder).getOrElse(v._1))
      )
    val duplicates = newPoses
      .filter(v => v._1 != v._2).toSeq
      .map(_._2)
      .groupBy(p => p).filter(_._2.size > 1).keySet
    newPoses.map(e => if (duplicates.contains(e._2)) e._1 else e._2)
  }

  val allDeltas = Range(-1, 2).flatMap(x => Range(-1, 2).map((x, _)))
    .filterNot(e => e._1 == 0 && e._2 == 0)
    .map(e => Position(e._1, e._2))
    .toSet

  def runSimulation(initElfPoses: Set[Position], initDirOrder: Seq[Direction]): (Set[Position], Num) = {
    var round10Poses: Set[Position] = Set()

    @tailrec
    def inner(elfPoses: Set[Position], dirOrder: Seq[Direction], round: Num): (Set[Position], Num) = {
      val simulated = simulate(elfPoses, dirOrder, allDeltas)
      if (round == 10)
        round10Poses = simulated

      if (simulated.equals(elfPoses))
        (simulated, round)
      else
        inner(simulated, dirOrder.drop(1).appended(dirOrder.head), round + 1)
    }

    val (_, rounds) = inner(initElfPoses, initDirOrder, 1)
    (round10Poses, rounds)
  }

  def getEmptyGroundTiles(elfs: Set[Position]): Num = {
    val minX = elfs.map(_.x).min
    val minY = elfs.map(_.y).min
    val maxX = elfs.map(_.x).max
    val maxY = elfs.map(_.y).max
    (maxY - minY + 1) * (maxX - minX + 1) - elfs.size
  }

  override def solve(input: String): String = {
    val elfPoses = parseMap(input)
    val directionOrder = Seq(Direction.N, Direction.S, Direction.W, Direction.E)
    val (simulated, rounds) = runSimulation(elfPoses, directionOrder)
    val p1 = getEmptyGroundTiles(simulated)
    val p2 = rounds
    s"P1: $p1\nP2: $p2"
  }
}

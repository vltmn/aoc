package io.vltmn.aoc
package y2022

class Day13 extends Solution {
  trait Packet

  case class List(including: Seq[Packet]) extends Packet

  case class Value(value: Int) extends Packet

  object PacketOrdering extends Ordering[Packet] {
    override def compare(x: Packet, y: Packet): Int = (x, y) match {
      case (List(Seq()), List(_)) => -1
      case (List(_), List(Seq())) => 1
      case (l: List, r: List) => l.including
        .zipAll(r.including, Value(-1), Value(-1))
        .map(e => compare(e._1, e._2))
        .dropWhile(_ == 0).headOption
        .getOrElse(-1)
      case (l, r: List) => compare(List(Seq(l)), r)
      case (l: List, r) => compare(l, List(Seq(r)))
      case (Value(l), Value(r)) => l.compare(r)
    }
  }


  override def solve(input: String): String = {
    val packets = input.split("\n\n")
      .map(_.split("\n"))
      .flatMap(e => Seq(e.head, e.last))

    def parse(part: String): List = {
      val containing = part
      val pre: String = containing.takeWhile(_ != '[')
      val preValues = pre
        .filter(e => e != ']' && e != '[')
        .split(",")
        .filter(_.nonEmpty)
        .map(_.toInt).map(Value)
      if (pre.length == containing.length) {
        return List(preValues)
      }
      val internalList = containing.drop(pre.length)
        .foldLeft((0, ""))((acc, curr) => curr match {
          case _ if (acc._1 == 0 && acc._2.nonEmpty) => acc
          case ']' if acc._1 == 1 => (0, acc._2 + ']')
          case ']' => (acc._1 - 1, acc._2 + ']')
          case '[' => (acc._1 + 1, acc._2 + '[')
          case _ => (acc._1, acc._2 + curr)
        })
        ._2
        .drop(1)
      val internalVals = parse(internalList)
      val post = containing.drop(containing.indexOf(internalList) + internalList.length) //.drop(2)
      val postValues = parse(post)
      val allVals = preValues.appended(internalVals).appendedAll(postValues.including)
      List(allVals)
    }

    val parsedPackets = packets
      .map(_.drop(1).dropRight(1))
      .map(parse)

    val pairs = parsedPackets.grouped(2).toSeq
    val evaluatedPairs = pairs
      .map(e => PacketOrdering.compare(e.head, e.last) < 0)

    val p1 = evaluatedPairs.zipWithIndex
      .filter(_._1)
      .map(_._2 + 1)
      .sum
    val dividerPackets = Seq("[[2]]", "[[6]]").map(parse)
    val sorted = parsedPackets.appendedAll(dividerPackets)
      .sorted(PacketOrdering)
    val indexA = sorted.indexOf(dividerPackets.head) + 1
    val indexB = sorted.indexOf(dividerPackets.last) + 1
    val p2 = indexA * indexB
    s"Part1:\n$p1\nPart2:\n$p2"
  }
}

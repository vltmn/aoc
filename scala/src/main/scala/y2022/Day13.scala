package io.vltmn.aoc
package y2022

class Day13 extends Solution {
  trait Packet

  case class Nested(including: Seq[Packet]) extends Packet {
    override def toString: String = "[" + including.map(_.toString).mkString(",") + "]"
  }

  case class Value(value: Int) extends Packet {
    override def toString: String = value.toString
  }

  object PacketOrdering extends Ordering[Packet] {
    override def compare(x: Packet, y: Packet): Int = (x, y) match {
      case (Nested(Seq()), Nested(_)) => -1
      case (Nested(_), Nested(Seq())) => 1
      case (l: Nested, r: Nested) => l.including
        .zipAll(r.including, Value(-1), Value(-1))
        .map(e => compare(e._1, e._2))
        .dropWhile(_ == 0).headOption
        .getOrElse(0)
      case (l, r: Nested) => compare(Nested(Seq(l)), r)
      case (l: Nested, r) => compare(l, Nested(Seq(r)))
      case (Value(l), Value(r)) => l.compare(r)
    }
  }

  def parse(part: String): Nested = {
    def inner(str: Seq[String], current: Seq[Packet]): (List[String], Nested) = str match {
      case "]" :: s =>
        (s, Nested(current))
      //case "," :: s => inner(s, current)
      case "[" :: s =>
        val (rest, nest) = inner(s, Seq())
        inner(rest, current.appended(nest))
      //(List(), inner(rest, current.appended(nest))._2)
      case x :: s =>
        inner(s, current.appended(Value(x.toInt)))
      case Seq() =>
        (List(), Nested(current))

    }

    val charArr = part.toCharArray
    val tokenized: List[String] = charArr.indices
      .foldLeft(List[String]())((acc, curr) => charArr(curr) match {
        case ',' => acc
        case c if (c.isDigit && curr > 0 && charArr(curr - 1).isDigit) => acc.dropRight(1).appended(acc.last + c)
        case c => acc.appended(c.toString)
      })
      .drop(1).dropRight(1)
    val (_, l) = inner(tokenized, Seq())
    l
  }

  override def solve(input: String): String = {
    val packets = input.split("\n\n")
      .map(_.split("\n"))
      .flatMap(e => Seq(e.head, e.last))

    val parsedPackets = packets
      .map(parse)

    val pairs = parsedPackets.grouped(2).toSeq
    val evaluatedPairs = pairs
      .map(e => PacketOrdering.compare(e.head, e.last) < 0)

    val p1 = evaluatedPairs.zipWithIndex
      .filter(_._1)
      .map(_._2 + 1)
      .sum
    val dividerPackets = Seq("[[2]]", "[[6]]").map(parse)
    val p2 = dividerPackets
      .map(dp => parsedPackets.count(pp => PacketOrdering.compare(pp, dp) < 0))
      .product
    s"Part1:\n$p1\nPart2:\n$p2"
  }
}

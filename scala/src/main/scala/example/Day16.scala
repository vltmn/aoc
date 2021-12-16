package io.vltmn.aoc2021
package example

class Day16 extends Solution {
  trait Value

  case class Packet(version: Int, typeId: Int, value: Value)

  case class Literal(value: Long) extends Value

  trait Operator extends Value {
    def subPackets: Seq[Packet]

    def evaluate(): Long
  }

  case class Sum(subPackets: Seq[Packet]) extends Operator {
    def evaluate(): Long = subpacketMapper(subPackets).sum
  }

  case class Product(subPackets: Seq[Packet]) extends Operator {
    override def evaluate(): Long = subpacketMapper(subPackets).product
  }

  case class Minimum(subPackets: Seq[Packet]) extends Operator {
    override def evaluate(): Long = subpacketMapper(subPackets).min
  }

  case class Maximum(subPackets: Seq[Packet]) extends Operator {
    override def evaluate(): Long = subpacketMapper(subPackets).max
  }

  case class GreaterThan(subPackets: Seq[Packet]) extends Operator {
    override def evaluate(): Long = {
      val packets = subpacketMapper(subPackets)
      if (packets.head > packets.last)
        1
      else
        0
    }
  }

  case class LessThan(subPackets: Seq[Packet]) extends Operator {
    override def evaluate(): Long = {
      val packets = subpacketMapper(subPackets)
      if (packets.head < packets.last)
        1
      else
        0
    }
  }

  case class EqualTo(subPackets: Seq[Packet]) extends Operator {
    override def evaluate(): Long = {
      val packets = subpacketMapper(subPackets)
      if (packets.head == packets.last)
        1
      else
        0
    }
  }

  val subpacketMapper: Seq[Packet] => Seq[Long] = packets => packets.map(p => p.value match {
    case Literal(v) => v
    case o: Operator => o.evaluate()
  })

  def hexToBinString(input: String): String = input.toCharArray
    .map(c => BigInt(c.toString, 16))
    .map(_.longValue.toBinaryString)
    .map(s => "0".repeat(4 - s.length) + s)
    .mkString

  def parseLiteralContent(data: String): (Literal, String) = {
    def isLastLiteralGroup(group: String): Boolean = group.startsWith("0")

    val grouped = data.grouped(5).toSeq
    val (pre, suf) = grouped.span(g => !isLastLiteralGroup(g))
    val groups = (pre ++ suf.take(1))
      .map(g => g.drop(1))
    val intValue = BigInt(groups.mkString, 2).longValue
    (Literal(intValue), suf.drop(1).mkString)
  }

  def parseOperatorContent(data: String): (Seq[Packet], String) = {
    def recurseBitLength(str: String, bitCount: Int): (Seq[Packet], String) = {
      val (packet, rest) = parseBinString(str)
      val nextBitCount = bitCount - (str.length - rest.length)
      if (nextBitCount == 0)
        (List(packet), rest)
      else {
        val (next, nextRest) = recurseBitLength(rest, nextBitCount)
        (List(packet) ++ next, nextRest)
      }
    }

    def recurseSubpacketCount(str: String, packetCount: Int): (Seq[Packet], String) = {
      val (packet, rest) = parseBinString(str)
      val nextPacketCount = packetCount - 1
      if (nextPacketCount == 0)
        (List(packet), rest)
      else {
        val (next, nextRest) = recurseSubpacketCount(rest, nextPacketCount)
        (List(packet) ++ next, nextRest)
      }
    }

    val lengthTypeId = Integer.parseInt(data.take(1), 2)
    val (subpackets, rest) = lengthTypeId match {
      case 0 =>
        // next 15 bits are length of subpackets
        val length = Integer.parseInt(data.slice(1, 16), 2)
        recurseBitLength(data.drop(16), length)
      case 1 => {
        // next 11 bits are number of subpackets
        val length = Integer.parseInt(data.slice(1, 12), 2)
        recurseSubpacketCount(data.drop(12), length)
      }
    }
    (subpackets, rest)
  }

  def parseBinString(data: String): (Packet, String) = {
    val version = Integer.parseInt(data.take(3), 2)
    val typeId = Integer.parseInt(data.slice(3, 6), 2)
    val content = data.drop(6)

    val (value, rest) = typeId match {
      case 4 => parseLiteralContent(content)
      case d => {
        val (subpackets, rest) = parseOperatorContent(content)
        val v = d match {
          case 0 => Sum(subpackets)
          case 1 => Product(subpackets)
          case 2 => Minimum(subpackets)
          case 3 => Maximum(subpackets)
          case 5 => GreaterThan(subpackets)
          case 6 => LessThan(subpackets)
          case 7 => EqualTo(subpackets)
        }
        (v, rest)
      }
    }
    (Packet(version, typeId, value), rest)
  }

  def unfoldVersions(data: Packet): Seq[Int] = data.value match {
    case o: Operator => List(data.version) ++ o.subPackets.flatMap(unfoldVersions)
    case _ => List(data.version)
  }

  def eval(packet: Packet): Long = {
    packet.value match {
      case Literal(v) => v
      case o: Operator => o.evaluate()
    }
  }

  def part1(input: String): Int = {
    val binString = hexToBinString(input)
    val (packet, _) = parseBinString(binString)
    val versions = unfoldVersions(packet)
    versions.sum
  }

  def part2(input: String): Long = {
    val binString = hexToBinString(input)
    val (packet, _) = parseBinString(binString)
    eval(packet)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)

    s"Part1: $p1\nPart2: $p2"
  }


}

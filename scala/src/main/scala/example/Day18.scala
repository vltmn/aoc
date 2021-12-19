package io.vltmn.aoc2021
package example

import example.Day18.Pair
import example.Day18._

class Day18 extends Solution {

  def parseInput(data: String): Seq[Pair] = data.linesIterator.map(parsePair).toSeq

  def pairSplitter(str: String): (String, String) = {
    val left = str.foldLeft("")((acc, curr) => {
      val unmatchedBraces = acc.count(_ == '[') - acc.count(_ == ']')
      if (acc.nonEmpty && unmatchedBraces == 0)
        acc
      else
        acc + curr
    })
    val right = str.drop(left.length + 1)
    (left, right)
  }

  def parsePair(data: String): Pair = {
    def parseSide(str: String): PairPart = if (str.contains("[")) parsePair(str) else Value(str.toInt)

    def inner(str: String): Pair = {
      val innerStr = str.slice(1, str.length - 1)
      val shouldRecurse = innerStr.contains("[")

      if (shouldRecurse) {
        val (left, right) = pairSplitter(innerStr)
        val leftPair = parseSide(left)
        val rightPair = parseSide(right)
        Pair(leftPair, rightPair)
      } else {
        val splitted = innerStr.split(",")
        Pair(Value(splitted.head.toInt), Value(splitted.last.toInt))
      }
    }

    inner(data)
  }

  def part1(input: String): Int = {
    val pairs = parseInput(input)
    val sum = pairs.drop(1).foldLeft(pairs.head)((acc, curr) => (acc + curr).reduce())
    sum.magnitude()
  }

  def part2(input: String): Int = {
    val pairs = parseInput(input)
   pairs
      .flatMap(p1 => pairs.filter(_ != p1).map(p2 => (p1, p2)))
      .flatMap(e => Seq(e._1 + e._2, e._2 + e._1))
      .distinct
      .map(_.reduce())
      .map(_.magnitude())
      .max
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

object Day18 {
  trait Direction

  case class Left() extends Direction

  case class Right() extends Direction

  type PairIndex = Seq[Direction]

  trait PairPart {
    def magnitude(): Int
  }

  case class Value(value: Int) extends PairPart {
    override def magnitude(): Int = value
  }

  case class Pair(left: PairPart, right: PairPart) extends PairPart {
    def indices(): Seq[PairIndex] = {
      val l: Seq[PairIndex] = left match {
        case Value(_) => Seq(Seq(new Left))
        case p: Pair => p.indices().map(_.prepended(new Left))
      }
      val r: Seq[PairIndex] = right match {
        case Value(_) => Seq(Seq(new Right))
        case p: Pair => p.indices().map(_.prepended(new Right))
      }
      l ++ r
    }

    def values(): Seq[Value] = {
      def extract(pp: PairPart) = pp match {
        case p: Pair => p.values()
        case v: Value => Seq(v)
      }

      val l = extract(left)
      val r = extract(right)
      l ++ r
    }

    def elementAt(pairIndex: PairIndex): Option[PairPart] = {
      def recurse(pp: PairPart, rest: PairIndex) = pp match {
        case _: Value if rest.nonEmpty => None
        case v: Value => Some(v)
        case p: Pair => p.elementAt(rest)
      }

      pairIndex match {
        case (_: Left) :: xs => recurse(left, xs)
        case (_: Right) :: xs => recurse(right, xs)
        case Nil => Some(this)
      }
    }

    def replaceAt(pairIndex: PairIndex, value: PairPart): Option[Pair] = {
      val x :: xs = pairIndex


      if (xs.isEmpty)
        x match {
          case _: Left => Some(Pair(value, right))
          case _: Right => Some(Pair(left, value))
        }
      else {
        val elem = x match {
          case _: Left => left
          case _: Right => right
        }
        elem match {
          case p: Pair => p.replaceAt(xs, value).map(p => x match {
            case _: Left => Pair(p, right)
            case _: Right => Pair(left, p)
          })
          case _ => None
        }
      }
    }

    def explode(index: PairIndex): Pair = {
      val elem = this.elementAt(index)
      val indices = this.indices()
      val (leftVal, rightVal) = elem match {
        case Some(Pair(Value(l), Value(r))) => (l, r)
      }
      //val (leftIdx, rightIdx) = indices.sliding(3).find(d => d(1).startsWith(index)).map(e => (e.head, e.last)).get
      val leftIdxOptional: Option[PairIndex] = indices.indexWhere(_.startsWith(index)) - 1 match {
        case x if x >= 0 => Some(indices(x))
        case _ => None
      }
      val rightIdxOptional: Option[PairIndex] = indices.lastIndexWhere(_.startsWith(index)) + 1 match {
        case x if x < indices.length => Some(indices(x))
        case _ => None
      }
      val currLeftVal: Int = leftIdxOptional.flatMap(leftIdx => this.elementAt(leftIdx).map {
        case Value(v) => v
      }).getOrElse(0)
      val currRightVal: Int = rightIdxOptional.flatMap(rightIdx => this.elementAt(rightIdx).map {
        case Value(v) => v
      }).getOrElse(0)

      replaceAt(index, Value(0))
        .flatMap(p => leftIdxOptional.flatMap(li => p.replaceAt(li, Value(leftVal + currLeftVal))).orElse(Some(p)))
        .flatMap(p => rightIdxOptional.flatMap(ri => p.replaceAt(ri, Value(rightVal + currRightVal))).orElse(Some(p)))
        .get
    }

    def split(index: PairIndex): Pair = {
      val elem = this.elementAt(index).get match {
        case Value(v) => v
      }
      val left = Math.floor(elem / 2.0).toInt
      val right = Math.ceil(elem / 2.0).toInt

      replaceAt(index, Pair(Value(left), Value(right))).get
    }

    def +(other: Pair): Pair = Pair(this, other)

    def reduce(): Pair = {
      val indices = this.indices()
      val toExplode = indices.find(i => i.size >= 5)
      val gt10 = this.values().zipWithIndex.find(e => e._1.value >= 10)
      if (toExplode.isDefined) {
        val idx = toExplode.get.dropRight(1)
        this.explode(idx).reduce()
      } else if (gt10.isDefined)
        this.split(indices(gt10.get._2)).reduce()
      else
        this
    }

    def magnitude(): Int = left.magnitude() * 3 + right.magnitude() * 2
  }
}

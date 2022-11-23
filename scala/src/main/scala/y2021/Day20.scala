package io.vltmn.aoc
package y2021

import y2021.Day20.Image

class Day20 extends Solution {
  def parseInput(input: String): (String, Image) = {
    val splitted = input.split("\n\n")
    val enhancementAlgo = splitted.head
    val map = splitted.last.linesIterator
      .map(_.toCharArray.toVector.map(c => if (c == '#') 1 else 0))
      .toVector
    (enhancementAlgo, Image(map))
  }

  def enhanceImage(enhancement: String, image: Image): Image = {
    val size = image.size
    val sizeIncrease = 2
    // 2 at each side
    val outputSize = size + sizeIncrease * 2
    val outputImage = Vector.fill(outputSize, outputSize)(0)

    val imageData = outputImage.zipWithIndex.map(e => e._1.zipWithIndex.map(e2 => {
      val x = e2._2 - sizeIncrease
      val y = e._2 - sizeIncrease
      val binString = (-1 to 1).flatMap(y => (-1 to 1).map(x => (x, y)))
        .map(pos => image.valueAt(pos._1 + x, pos._2 + y))
        .map(i => i.toString)
        .mkString
      val idx = Integer.parseInt(binString, 2)
      if (enhancement.charAt(idx) == '#') 1 else 0
    }))
    val baseValue = image.baseValue match {
      case 0 => if (enhancement.head == '#') 1 else 0
      case 1 => if (enhancement.last == '.') 0 else 1
    }
    Image(imageData, baseValue)
  }

  def part1(input: String): Int = {
    val (enhancement, image) = parseInput(input)
    val processed = (0 until 2).foldLeft(image)((acc, _) => enhanceImage(enhancement, acc))
    processed.litPixelCount
  }

  def part2(input: String): Int = {
    val (enhancement, image) = parseInput(input)
    val processed = (0 until 50).foldLeft(image)((acc, _) => enhanceImage(enhancement, acc))
    processed.litPixelCount
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part1: $p1\nPart2: $p2"
  }
}

object Day20 {
  type ImageData = Vector[Vector[Int]]

  case class Image(data: ImageData, baseValue: Int = 0) {
    def size: Int = data.size
    def litPixelCount: Int = data.flatten.count(_ == 1)

    def valueAt(x: Int, y: Int): Int = {
      if (x >= 0 && x < size && y >= 0 && y < size)
        data(y)(x)
      else
        baseValue
    }
  }
}

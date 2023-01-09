package io.vltmn.aoc
package util

import io.vltmn.aoc.util.SeqUtils.Utils
import org.scalatest.funsuite.AnyFunSuite

class SeqUtilsTest extends AnyFunSuite {
  private val arr = Seq("a", "b", "c", "d")
  test("without at idx 0") {
    val expected = Seq("b", "c", "d")
    assert(arr.without(0) == expected)
  }
  test("without at end index") {
    val expected = Seq("a", "b", "c")
    assert(arr.without(arr.size - 1) == expected)
  }
  test("without at other index") {
    val expected = Seq("a", "c", "d")
    assert(arr.without(1) == expected)
  }
  test("with at idx 0") {
    val expected = Seq("s", "a", "b", "c", "d")
    assert(arr.withElem(0, "s") == expected)
  }
  test("with at end") {
    val expected = Seq("a", "b", "c", "d", "s")
    assert(arr.withElem(arr.size, "s") == expected)
  }
  test("with at other idx") {
    val expected = Seq("a", "b", "s", "c", "d")
    assert(arr.withElem(2, "s") == expected)
  }
}

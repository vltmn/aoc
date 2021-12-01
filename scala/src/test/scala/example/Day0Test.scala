package io.vltmn.aoc2021
package example

import org.scalatest.funsuite.AnyFunSuite

class Day0Test extends AnyFunSuite {
  val day0 = new Day0()
  test("Day0.isAbba 4 chars") {
    assert(day0.isAbba("abba"))
    assert(day0.isAbba("alla"))
  }
  test("Day0.isAbba more chars") {
    assert(day0.isAbba("caccafsd"))
  }

  test("Day0.supportsTLS") {
    assert(day0.supportsTLS("abba[mnop]qrst"))
    assert(!day0.supportsTLS("abcd[bddb]xyyx"))
    assert(!day0.supportsTLS("aaaa[qwer]tyui"))
    assert(day0.supportsTLS("ioxxoj[asdfgh]zxcvbn"))
  }
}

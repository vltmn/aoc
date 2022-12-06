package io.vltmn.aoc
package util

object MatrixImprovements {
  implicit class MatrixImprovements[A](val m: Seq[Seq[A]]) {
    def squarePad(elem: A): Seq[Seq[A]] = {
      val maxLen = m.map(_.length).max
      m.map(_.padTo(maxLen, elem))
    }
  }
}


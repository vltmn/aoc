package io.vltmn.aoc
package util

object SeqUtils {
  implicit class Utils[T](val s: Seq[T]) {
    def without(idx: Int): Seq[T] = (s take idx ) ++ (s drop (idx + 1))

    def withElem(idx: Int, elem: T): Seq[T] = (s take idx).appended(elem) ++ (s drop idx)
  }
}


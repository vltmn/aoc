package io.vltmn.aoc

import util.InputReader

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}

object Main extends App {
  val solution: Solution = new y2023.Day1()
  val day: Int = solution.getClass.getSimpleName.substring(3).toInt
  val year = solution.getClass.getPackageName.split("\\.").last.substring(1).toInt
  val reader = new InputReader(year, day)
  val data = reader.read()
  val solveFuture = Future(solution.solve(data))
  val pre = System.nanoTime()
  val output = Await.result(solveFuture, Duration(90000, SECONDS))
  val execTime = System.nanoTime() - pre
  println(s"Output was:\n${output}")
  val execTimeFormatted = execTime.toDouble / Math.pow(10, 9)
  println(s"Execution time: $execTimeFormatted s")
}

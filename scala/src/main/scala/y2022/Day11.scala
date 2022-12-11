package io.vltmn.aoc
package y2022

class Day11 extends Solution {
  type MonkeyData = Long

  case class MonkeyNote(id: Int, op: (MonkeyData) => MonkeyData, testDiv: MonkeyData, ifTrue: Int, ifFalse: Int)

  type MonkeyState = Seq[MonkeyData]

  def simMonkey(monkey: MonkeyNote, state: Seq[MonkeyState], truncOp: (MonkeyData) => MonkeyData): Seq[MonkeyState] = {
    val myState = state(monkey.id)
    myState.foldLeft(state)((acc, curr) => {
      val newCurr = truncOp(monkey.op(curr))
      val receiver = if (newCurr % monkey.testDiv == 0) monkey.ifTrue else monkey.ifFalse
      acc
        .updated(receiver, acc(receiver).appended(newCurr))
        .updated(monkey.id, acc(monkey.id).drop(1))
    })
  }

  def simulate(monkeyNotes: Seq[MonkeyNote])
              (initState: Seq[MonkeyState], initData: Seq[MonkeyData])
              (rounds: Int, truncOp: MonkeyData => MonkeyData): MonkeyData = {
    val monkeysCount = initState.size
    val init: (Seq[MonkeyState], Seq[MonkeyData]) = (initState, initData)
    (0 until rounds * monkeysCount).map(_ % monkeysCount)
      .foldLeft(init)((acc, curr) => {
        val updatedInspections: Seq[Long] = acc._2.updated(curr, acc._2(curr) + acc._1(curr).length)
        val newState = simMonkey(monkeyNotes(curr), acc._1, truncOp)
        (newState, updatedInspections)
      })
      ._2.sorted.reverse
      .take(2).product
  }

  override def solve(input: String): String = {
    val pattern = ("Monkey (\\d):\\s+?" +
      "Starting items: ([\\d\\s,]+)\\s+" +
      "Operation: new = old ([\\*\\+] .+)\\s+" +
      "Test: divisible by (\\d+)\\s+" +
      "If true: throw to monkey (\\d+)\\s+" +
      "If false: throw to monkey (\\d+)").r
    val splitted = input.split("\n\n")
    val parsed = splitted
      .map(pattern.findFirstMatchIn(_))
      .zipWithIndex
      .map(e => e._1.get)
      .map(e => {
        val splitOp = e.group(3).split(" ").toSeq
        val op = splitOp match {
          case Seq("*", "old") => (v: MonkeyData) => v * v
          case Seq("+", "old") => (_: MonkeyData) * 2
          case Seq("+", o) => (_: MonkeyData) + o.toLong
          case Seq("*", o) => (_: MonkeyData) * o.toLong
        }
        val div = e.group(4).toLong
        val items = e.group(2).split(",").map(_.trim).map(_.toLong).toSeq
        (MonkeyNote(e.group(1).toInt, op, div, e.group(5).toInt, e.group(6).toInt), items)
      })
    val monkeyNotes = parsed.map(_._1)
    val initState = parsed.map(_._2).toSeq
    val initData = parsed.map(_ => 0L).toSeq
    val simulator = simulate(monkeyNotes)(initState, initData)(_, _)
    val p1 = simulator(20, _ / 3)
    val magic = parsed.map(_._1.testDiv).product
    val p2 = simulator(10000, _ % magic)
    s"Part1:\n$p1\nPart2:\n$p2"
  }
}

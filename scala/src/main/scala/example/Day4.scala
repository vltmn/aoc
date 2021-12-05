package io.vltmn.aoc2021
package example

class Day4 extends Solution {
  val size = 5
  type EvalDecider = (Seq[Board], Int, Seq[Board]) => Option[(Int, Board)]
  type NewBoardMapper = (Seq[Board], Seq[Board]) => Seq[Board]

  def parseInput(input: String): (Seq[Int], Seq[Board]) = {
    val lines = input.linesIterator.toList
    val nbrs = lines.head.split(",").map(_.toInt)
    val boards = lines.drop(2)
      .grouped(size + 1)
      .map(_.take(size).flatMap(_.split(" ").filter(!_.isBlank).map(_.toInt)))
      .map(brd => brd.map(UnmarkedCell))
      .toSeq
    (nbrs, boards)
  }

  def evalValues(decider: EvalDecider)(mapper: NewBoardMapper)(nmbrs: Seq[Int], boards: Seq[Board]): (Int, Board) = {
    val nbr = nmbrs.head
    val newBoards = boards.map(b => b
      .map {
        case UnmarkedCell(v) if v == nbr => MarkedCell(v)
        case MarkedCell(v) => MarkedCell(v)
        case UnmarkedCell(v) => UnmarkedCell(v)
      }
    )
    val wonBoards = newBoards.filter(brd => hasWon(brd))
    decider(wonBoards, nbr, newBoards) match {
      case Some(d) => d
      case None => evalValues(decider)(mapper)(nmbrs.drop(1), mapper(wonBoards, newBoards))
    }
  }

  def hasWon(board: Board): Boolean = {
    val lineHasWon = (l: Seq[Cell]) => l.count(_ match {
      case UnmarkedCell(_) => true
      case MarkedCell(_) => false
    }) == 0
    val hSeries = board.grouped(size).toSeq
    val vSeries = hSeries.transpose
    (hSeries ++ vSeries).exists(lineHasWon)
  }

  def evalBoardScore(board: Board): Int = board.filter({
    case UnmarkedCell(_) => true
    case MarkedCell(_) => false
  }).map(_.value).sum

  def part1(input: String): Int = {
    val (nmbrs, boards) = parseInput(input)
    val decider: EvalDecider = (wonBoards, nbr, _) => wonBoards.headOption.map((nbr, _))
    val mapper: NewBoardMapper = (_, nb) => nb
    val (wonNbr, wonBoard) = evalValues(decider)(mapper)(nmbrs, boards)
    wonNbr * evalBoardScore(wonBoard)
  }

  def part2(input: String): Int = {
    val (nmbrs, boards) = parseInput(input)
    val decider: EvalDecider = (wonBoards, nbr, newBoards) => {
      newBoards.filter(!wonBoards.contains(_)) match {
        case Nil => wonBoards.headOption.map((nbr, _))
        case _ => None
      }
    }
    val mapper: NewBoardMapper = (wonBoards, newBoards) => newBoards.filter(!wonBoards.contains(_))
    val (wonNbr, wonBoard) = evalValues(decider)(mapper)(nmbrs, boards)
    wonNbr * evalBoardScore(wonBoard)
  }

  override def solve(input: String): String = {
    val p1 = part1(input)
    val p2 = part2(input)
    s"Part 1: ${p1}\nPart 2: ${p2}"
  }

  trait Cell {
    def value: Int
  }

  case class UnmarkedCell(value: Int) extends Cell

  case class MarkedCell(value: Int) extends Cell

  type Board = Seq[Cell]
}

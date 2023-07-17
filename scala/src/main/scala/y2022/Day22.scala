package io.vltmn.aoc
package y2022

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

class Day22 extends Solution {
  case class Position(x: Int, y: Int) {
    def +(other: Position): Position = Position(x + other.x, y + other.y)
  }

  object Position {
    def apply(pair: (Int, Int)): Position = new Position(pair._1, pair._2)
  }

  trait Direction {
    def left(): Direction

    def right(): Direction

    def reverse(): Direction = right().right()

    def horizontal(): Boolean = score() % 2 == 0

    def increasing(): Boolean = score() < 2

    def decreasing(): Boolean = !increasing()

    def vertical(): Boolean = !horizontal()

    def score(): Int
  }

  case class Left() extends Direction {
    override def left(): Direction = Down()

    override def right(): Direction = Up()

    override def score(): Int = 2
  }

  case class Right() extends Direction {
    override def left(): Direction = Up()

    override def right(): Direction = Down()

    override def score(): Int = 0
  }

  case class Up() extends Direction {
    override def left(): Direction = Left()

    override def right(): Direction = Right()

    override def score(): Int = 3
  }

  case class Down() extends Direction {
    override def left(): Direction = Right()

    override def right(): Direction = Left()

    override def score(): Int = 1
  }

  case class Move(count: Int) extends PathComponent

  case class LeftTurn() extends PathComponent

  case class RightTurn() extends PathComponent

  trait PathComponent

  type BoardPosition = (Position, Direction)

  trait BoardCell

  case class OpenCell() extends BoardCell

  case class WallCell() extends BoardCell

  type BoardMap = Map[Position, BoardCell]

  def parseDirections(str: String): Seq[PathComponent] = str.toCharArray
    .foldLeft(Seq[String]())((acc, curr) => curr match {
      case _ if acc.isEmpty => acc.appended(curr + "")
      case n if n.isDigit && acc.last.charAt(0).isDigit => acc.dropRight(1).appended(acc.last + n)
      case s => acc.appended(s + "")
    })
    .map {
      case "L" => LeftTurn()
      case "R" => RightTurn()
      case s => Move(s.toInt)
    }

  def parseMap(str: String): (BoardMap, BoardPosition) = {
    val map = str.split("\n").zipWithIndex
      .flatMap {
        case (l, y) => l.toCharArray.zipWithIndex
          .filter(!_._1.isWhitespace)
          .map {
            case ('.', x) => (Position(x, y), OpenCell())
            case ('#', x) => (Position(x, y), WallCell())
          }
      }
      .toMap
    val initPos = map.keys.min((a: Position, b: Position) => if (a.y < b.y)
      -1
    else if (a.y == b.y)
      if (a.x < b.x)
        -1 else 1
    else 1)
    (map, (initPos, Right()))
  }

  type PosMapper = Position => Int

  def toSortedMap(first: PosMapper, second: PosMapper, map: BoardMap): SortedMap[Int, SortedMap[Int, BoardCell]] = {
    TreeMap.from(map.toSeq.map {
      case (p, c) => (first(p), (second(p), c))
    }.groupMap(_._1)(_._2).view.mapValues(v => TreeMap.from(v)))
  }

  def applyDirections(map: BoardMap, initialPosition: BoardPosition, directions: Seq[PathComponent], wrapFn: BoardPosition => BoardPosition): BoardPosition = {
    val deltas: Map[Direction, Position] = Map(
      (Right(), Position(1, 0)),
      (Down(), Position(0, 1)),
      (Left(), Position(-1, 0)),
      (Up(), Position(0, -1))
    )

    def move(pos: BoardPosition): BoardPosition = {
      val posWDelta = pos._1 + deltas(pos._2)
      val mapCell = map.get(posWDelta)
      if (mapCell.isEmpty) {
        val wrapped = wrapFn(pos)
        if (map(wrapped._1) == WallCell())
          pos
        else
          wrapped
      }
      else if (mapCell.contains(WallCell()))
        pos
      else
        (posWDelta, pos._2)
    }

    @tailrec
    def applyMove(count: Int, pos: BoardPosition): BoardPosition = count match {
      case 0 => pos
      case n =>
        val next = move(pos)
        if (next == pos)
          next
        else
          applyMove(n - 1, next)
    }

    @tailrec
    def inner(pos: BoardPosition, directions: Seq[PathComponent]): BoardPosition = directions match {
      case LeftTurn() :: rest => inner((pos._1, pos._2.left()), rest)
      case RightTurn() :: rest => inner((pos._1, pos._2.right()), rest)
      case Move(mc) :: rest =>
        inner(applyMove(mc, pos), rest)
      case Seq() => pos
    }

    inner(initialPosition, directions)
  }

  def mkPart1Wrap(map: BoardMap): BoardPosition => BoardPosition = {
    val xMap = toSortedMap(_.x, _.y, map)
    val yMap = toSortedMap(_.y, _.x, map)
    pos => {
      val useHead = pos._2.increasing()
      val horizontal = pos._2.horizontal()
      val mapToUse = if (horizontal) yMap(pos._1.y) else xMap(pos._1.x)
      val pp = (if (useHead) mapToUse.head else mapToUse.last)._1
      val p: Position = Position(if (horizontal) (pp, pos._1.y) else (pos._1.x, pp))
      (p, pos._2)
    }
  }

  def mkPart2Wrap(map: BoardMap): BoardPosition => BoardPosition = {
    val faceSize = toSortedMap(_.x, _.y, map)
      .view.mapValues(_.size).values.min
    val toFacePos: Position => Position = p => Position(p.x / faceSize, p.y / faceSize)
    val fromFacePos: Position => Position = p => Position(p.x * faceSize, p.y * faceSize)
    val faces = map.keys.map(toFacePos).toSet
    val faceOrd: Ordering[Position] = (a, b) => Ordering.Tuple2[Int, Int].compare((a.y, a.x), (b.y, b.x))
    val faceIdxPosMap = faces.toSeq.sorted(faceOrd).zipWithIndex.map(_.swap).toMap
    val posFaceIdxMap = faceIdxPosMap.view.map(_.swap).toMap

    pos => {
      val startFacePos = toFacePos(pos._1)
      val startFaceIdx = posFaceIdxMap(startFacePos)
      val result = wrapMap((startFaceIdx + 1, pos._2))
      val nextFacePos = faceIdxPosMap(result._1 - 1)
      val nextFaceTopLeft = fromFacePos(nextFacePos)
      val nextDirection = result._2.reverse()

      if (result._2 == pos._2 && nextDirection.horizontal()) {
        val nextX = if (result._2.increasing()) nextFaceTopLeft.x + faceSize - 1 else nextFaceTopLeft.x
        val nextY = nextFaceTopLeft.y + (faceSize - pos._1.y % faceSize) - 1
        (Position(nextX, nextY), nextDirection)
      } else if (nextDirection == pos._2 && nextDirection.vertical()) {
        val nextX = pos._1.x % faceSize + nextFaceTopLeft.x
        val nextY = if (result._2.increasing()) nextFaceTopLeft.y + faceSize - 1 else nextFaceTopLeft.y
        (Position(nextX, nextY), nextDirection)
      } else {
        val nextX = pos._1.y % faceSize + nextFaceTopLeft.x
        val nextY = pos._1.x % faceSize + nextFaceTopLeft.y
        (Position(nextX, nextY), nextDirection)
      }
    }
  }
  def getPassword(pos: BoardPosition): Int = (pos._1.y + 1) * 1000 + (pos._1.x + 1) * 4 + pos._2.score()

  val wrapMap: Map[(Int, Direction), (Int, Direction)] = Map.from(Seq(
    ((1, Up()), (6, Left())),
    ((1, Left()), (4, Left())),
    ((2, Up()), (6, Down())),
    ((2, Right()), (5, Right())),
    ((2, Down()), (3, Right())),
    ((3, Left()), (4, Up())),
    ((5, Down()), (6, Right()))
  ).flatMap(kv => Seq(kv, kv.swap)))

  override def solve(input: String): String = {
    val splitted = input.split("\n\n")
    val (map, initialPosition) = parseMap(splitted.head)
    val directions = parseDirections(splitted.last)
    val run = (applyDirections _).curried(map)(initialPosition)(directions)
    val p1 = getPassword(run(mkPart1Wrap(map)))
    val p2 = getPassword(run(mkPart2Wrap(map)))
    s"P1: $p1\nP2: $p2"
  }
}

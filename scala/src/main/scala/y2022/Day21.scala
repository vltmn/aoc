package io.vltmn.aoc
package y2022

import scala.annotation.tailrec

class Day21 extends Solution {
  type Num = Long

  trait Node {
    def evaluate: Num

    def name: String

    def nodeIncluded: Set[String]
  }

  case class BinOpNode(op: (Num, Num) => Num, children: (Node, Node), name: String) extends Node {
    override def evaluate: Num = op(children._1.evaluate, children._2.evaluate)

    override def nodeIncluded: Set[String] = children._1.nodeIncluded union children._2.nodeIncluded union Set(name)

  }

  case class LeafNode(value: Num, name: String) extends Node {
    override def evaluate: Num = value

    override def nodeIncluded: Set[String] = Set(name)

  }

  object Operators {
    type BinaryOperator = (Num, Num) => Num
    val AddOp: BinaryOperator = _ + _
    val SubOp: BinaryOperator = _ - _
    val DivOp: BinaryOperator = _ / _
    val MulOp: BinaryOperator = _ * _
  }

  type MonkeyMap = Map[String, Seq[String]]

  def genTree(map: MonkeyMap, name: String = "root"): Node = map(name) match {
    case Seq(v) => LeafNode(v.toInt, name)
    case Seq(a, op, b) =>
      val operator: (Num, Num) => Num = op match {
        case "+" => Operators.AddOp
        case "-" => Operators.SubOp
        case "/" => Operators.DivOp
        case "*" => Operators.MulOp
      }
      BinOpNode(operator, (genTree(map, a), genTree(map, b)), name)
  }

  def solveForVal(root: Node, nodeName: String): Num = {
    @tailrec
    def inner(tree: Node, wantedVal: Num): Num = tree match {
      case LeafNode(_, name) if (name == nodeName) => wantedVal
      case BinOpNode(op, (a, b), _) =>
        val (includingNode, notIncludingNode) = if (a.nodeIncluded.contains(nodeName)) (a, b) else (b, a)
        val otherValue = notIncludingNode.evaluate
        op match {
          case Operators.AddOp => inner(includingNode, wantedVal - otherValue)
          case Operators.MulOp => inner(includingNode, wantedVal / otherValue)
          case Operators.SubOp => inner(includingNode,
            if (includingNode == a)
              wantedVal + otherValue
            else otherValue - wantedVal
          )
          case Operators.DivOp => inner(includingNode,
            if (includingNode == a)
              wantedVal * otherValue
            else
              otherValue / wantedVal
          )
        }
    }

    val (myRoot, otherRoot) = root match {
      case BinOpNode(_, (a, b), _) => if (a.nodeIncluded.contains(nodeName)) (a, b) else (b, a)
    }
    val wanted = otherRoot.evaluate
    inner(myRoot, wanted)
  }

  override def solve(input: String): String = {
    val parsedMap = input
      .split("\n")
      .map(_.split(":").map(_.trim))
      .map(e => (e.head, e.last.split(" ").toSeq)).toMap
    val tree = genTree(parsedMap)
    val p1 = tree.evaluate
    val me = "humn"
    val p2 = solveForVal(tree, me)
    s"Part1: \n$p1\nPart2: \n$p2"
  }
}

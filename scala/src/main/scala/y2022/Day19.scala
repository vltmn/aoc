package io.vltmn.aoc
package y2022

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

class Day19 extends Solution {
  trait Resource

  case class Ore() extends Resource

  case class Clay() extends Resource

  case class Obsidian() extends Resource

  case class Geode() extends Resource

  case class Balance(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0) {
    def toResourceMap: Map[Resource, Int] = Map((Ore(), ore), (Clay(), clay), (Obsidian(), obsidian), (Geode(), geode))

    private def mergeWithOp(other: Balance, op: (Int, Int) => Int): Balance =
      Balance(op(ore, other.ore), op(clay, other.clay), op(obsidian, other.obsidian), op(geode, other.geode))

    def add(other: Balance): Balance = mergeWithOp(other, _ + _)

    def sub(other: Balance): Balance = mergeWithOp(other, _ - _)

    def isPositive: Boolean = ore >= 0 && clay >= 0 && obsidian >= 0
  }

  object Balance {
    def fromResourceMap(res: Map[Resource, Int]): Balance =
      Balance(res.getOrElse(Ore(), 0), res.getOrElse(Clay(), 0), res.getOrElse(Obsidian(), 0), res.getOrElse(Geode(), 0))
  }

  type CurrentRobots = Map[Resource, Int]

  type State = (Balance, CurrentRobots)

  case class Blueprint(id: Int, oreRobotCost: Balance, clayRobotCost: Balance, obsidianRobotCost: Balance, geodeRobotCost: Balance) {
    def toCostMap: Map[Resource, Balance] = Map((Ore(), oreRobotCost), (Clay(), clayRobotCost), (Obsidian(), obsidianRobotCost), (Geode(), geodeRobotCost))
  }

  def maximizeYield(blueprint: Blueprint, resourceToMaximize: Resource, time: Int): (Balance, CurrentRobots) = {
    val maxCosts = blueprint.toCostMap.values
      .reduce((a, b) => Balance(a.ore max b.ore, a.clay max b.clay, a.obsidian max b.obsidian, a.geode max b.geode))
      .toResourceMap

    def incOptional(o: Option[Int]): Option[Int] = Some(o.getOrElse(0) + 1)

    def inner(state: State, currentTime: Int): State = currentTime match {
      case 0 => (state._1 add Balance.fromResourceMap(state._2), state._2)
      case _ =>
        val bots = state._2
        val balance = state._1

        val nextStates = if ((balance sub blueprint.geodeRobotCost).isPositive) {
          // if possible to build geode robot, do that
          Seq((balance sub blueprint.geodeRobotCost, bots.updatedWith(Geode())(incOptional)))
        } else {
          val obsidianBot = if (
            bots.getOrElse(Obsidian(), 0) < maxCosts(Obsidian()) &&
              currentTime > 1
          )
            Some((balance sub blueprint.obsidianRobotCost, bots.updatedWith(Obsidian())(incOptional)))
          else None

          val clayBot = if (
            bots.getOrElse(Clay(), 0) < maxCosts.getOrElse(Clay(), 0) &&
              currentTime > 2
          )
            Some((balance sub blueprint.clayRobotCost, bots.updatedWith(Clay())(incOptional)))
          else None

          val oreBot = if (
            bots.getOrElse(Ore(), 0) < maxCosts(Ore()) &&
              currentTime > 2
          )
            Some((balance sub blueprint.oreRobotCost, bots.updatedWith(Ore())(incOptional)))
          else None

          val justHarvest = if (balance.ore < maxCosts(Ore()) || (oreBot.isEmpty && clayBot.isEmpty))
            Some((balance, bots))
          else None
          Seq(obsidianBot, clayBot, oreBot, justHarvest)
            .filter(_.isDefined)
            .map(_.get)
            .filter(_._1.isPositive)
        }

        val next = nextStates
          .map(e => (e._1.add(Balance.fromResourceMap(bots)), e._2))
          .map(e => inner(e, currentTime - 1))
        next.maxBy(a => a._1.toResourceMap(resourceToMaximize))
    }
    inner((Balance(), Map((Ore(), 1))), time - 1)
  }

  override def solve(input: String): String = {
    val pattern = "Blueprint (\\d+). Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.".r
    val blueprints = input
      .linesIterator
      .map(pattern.findFirstMatchIn)
      .map(_.get)
      .map(_.subgroups.map(_.toInt))
      .map(g => Blueprint(g(0), Balance(g(1)), Balance(g(2)), Balance(g(3), g(4)), Balance(g(5), obsidian = g(6))))
      .toSeq

    val p1 = blueprints.par
      .map(b => (b.id, maximizeYield(b, Geode(), 24)))
      .map(e => e._1 * e._2._1.geode)
      .sum
    val p2 = blueprints.take(3).par
      .map(b => maximizeYield(b, Geode(), 32))
      .map(_._1.geode)
      .product
    s"Part1:\n$p1\nPart2:\n$p2"
  }


}

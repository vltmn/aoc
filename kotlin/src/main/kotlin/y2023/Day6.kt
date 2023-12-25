package y2023

import Solution
import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.pow
import kotlin.math.sqrt


data class Race(val time: Double, val record: Double)
class Day6 : Solution {

    override fun solve(input: String): String {
        val inp2 = "Time:      7  15   30\n" +
                "Distance:  9  40  200"

        val filteredLines = input.split("\n")
            .map { l -> l.split(" ").drop(1).filter { it.isNotBlank() } }
        val races = filteredLines
            .let { l -> l.first().zip(l.last()).map { Race(it.first.toDouble(), it.second.toDouble()) } }
        val p2Race = filteredLines
            .map { it.reduce {a, b -> a + b}.toDouble() }
            .let { p -> Race(p.first(), p.last()) }

        // dst traveled: f(x, y)= x*(y-x)=yx-x^2
        // solve for x, bx-x^2>a, b time, a record
        // x = (b+/-sqrt(b^2-4a))/2

        fun extractWinningPossibilities(race: Race): List<Int> {
            val root = sqrt(race.time.pow(2)-4*race.record)
            val min = ceil((race.time - root)/2).toInt()
            val max = floor((race.time + root)/2).toInt()
            fun cond(n: Int): Boolean =  n * (race.time - n) <= race.record
            return (min..max).toList()
                .dropWhile(::cond)
                .dropLastWhile(::cond)
        }
        val p1 = races.map(::extractWinningPossibilities)
            .map { it.size }
            .reduce {a, b -> a * b}

        val p2 = extractWinningPossibilities(p2Race).size
        return "Part1: $p1\nPart2: $p2"
    }
}
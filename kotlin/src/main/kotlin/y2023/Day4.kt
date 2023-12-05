package y2023

import Solution
import kotlin.math.pow

class Day4 : Solution {
    override fun solve(input: String): String {
        val inp2 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n" +
                "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n" +
                "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n" +
                "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n" +
                "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n" +
                "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

        fun digitSplit(str: String): List<Int> = str.split(" ")
            .filter { it.trim().isNotEmpty() }
            .map { it.trim().toInt() }

        val parsed = input.lines()
            .map { l ->
                val (pre, mineStr) = l.split("|")
                val (_, winStr) = pre.split(":")
                Pair(digitSplit(winStr).toSet(), digitSplit(mineStr).toSet())
            }


        val p1 = parsed.sumOf { sc ->
            when (val intersectionSize = sc.second.intersect(sc.first).size) {
                0 -> 0.0
                else -> 2.0.pow(intersectionSize - 1)
            }
        }
        val p2 = parsed.fold(Pair(0, emptyList<Int>())) { acc, curr ->
            val qty = (acc.second.firstOrNull() ?: 0) + 1
            val wins = curr.second.intersect(curr.first).size
            // add to the list of wins
            val existing = acc.second.drop(1)
            val new = List(size = wins) { 1 * qty }
            val newQtys = (1 .. existing.size.coerceAtLeast(new.size))
                .mapIndexed { i, _ -> (existing.getOrNull(i) ?: 0) + (new.getOrNull(i) ?: 0) }
            Pair(qty + acc.first, newQtys)
        }
        return "Part1: $p1\nPart2: $p2"
    }
}
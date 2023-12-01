package y2023

import Solution
import util.NumberUtil

class Day1 : Solution {

    override fun solve(input: String): String {
        val lines = input.lines()

        fun intMerge(vals: List<Int>) = vals.first() * 10 + vals.last()

        val p1 = lines.map { line ->
            line.filter { it.isDigit() }.map { it.digitToInt() }
        }.sumOf(::intMerge)


        val p2 = lines.map { line ->
            NumberUtil.DigitMap.entries
                .fold(line) {l, e -> l.replace(e.key, e.key + e.value + e.key)}
                .filter { it.isDigit() }
                .map { it.digitToInt() }
        }.sumOf(::intMerge)

        return "Part1: $p1\nPart2: $p2"
    }
}
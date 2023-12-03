package y2023

import Solution
import kotlin.math.absoluteValue

data class Position(val x: Int, val y: Int) {
    fun isAdjacentTo(other: Position): Boolean = (x - other.x).absoluteValue < 2 && (y - other.y).absoluteValue < 2
    fun adjacentPositions(): Set<Position> =
        IntRange(-1, 1).flatMap { x -> IntRange(-1, 1).map { Position(x + this.x, it + this.y) } }
            .filter { it != this }
            .toSet()
}

data class PartNum(val pos: Position, val value: Int) {
    fun isAdjacentTo(other: Position): Boolean =
        pos.isAdjacentTo(other) || Position(pos.x + value.toString().length - 1, pos.y).isAdjacentTo(other)

    fun extendWith(digit: Int) = PartNum(pos, value * 10 + digit)
}

class Day3 : Solution {
    override fun solve(input: String): String {
        val inp2 = "467..114..\n" +
                "...*......\n" +
                "..35..633.\n" +
                "......#...\n" +
                "617*......\n" +
                ".....+.58.\n" +
                "..592.....\n" +
                "......755.\n" +
                "...\$.*....\n" +
                ".664.598.."


        val parsed = input.lines().map { it.toCharArray() }

        val indexed = parsed.map { it.withIndex() }.withIndex()
        val width = indexed.maxOf { it.index }
        val height = indexed.first().value.maxOf { it.index }
        val symbols = indexed.flatMap { l ->
            l.value
                .filter { it.value.isSymbol() }
                .map { Position(it.index, l.index) }
        }
        val gearSymbols = indexed.flatMap { l ->
            l.value
                .filter { it.value == '*' }
                .map { Position(it.index, l.index) }
        }
        val numbers = indexed.flatMap { l ->
            l.value
                .fold(emptyList<PartNum>()) { acc, curr ->
                    if (curr.value.isDigit()) {
                        val pos = Position(curr.index, l.index)
                        val digit = curr.value.digitToInt()
                        if (acc.lastOrNull()?.isAdjacentTo(pos) == true) {
                            acc.dropLast(1) + acc.last().extendWith(digit)
                        } else acc + PartNum(pos, digit)
                    } else acc
                }
        }
        val partNumbers = symbols.flatMap { s ->
            s.adjacentDigits(parsed, width, height)
        }.mapNotNull { p -> numbers.find { it.isAdjacentTo(p) } }
            .toSet()

        val p1 = partNumbers.sumOf { it.value }
        val gears = gearSymbols.map { s ->
            val nums = s.adjacentDigits(parsed, width, height)
                .mapNotNull { p -> numbers.find { it.isAdjacentTo(p) } }
            nums.toSet().let { if (it.size != 2) emptySet() else it }
        }.filter { it.size == 2 }
        val p2 = gears.sumOf { pns -> pns.first().value * pns.last().value }
        return "Part1: $p1\nPart2: $p2"
    }
}

private fun Position.adjacentDigits(parsed: List<CharArray>, width: Int, height: Int): List<Position> =
    this.adjacentPositions().filter { it.x in 0..width && it.y in 0..height }
        .filter { parsed[it.y][it.x].isDigit() }

private fun Char.isSymbol(): Boolean = !this.isDigit() && this != '.'
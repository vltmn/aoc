package y2023

import Solution
import sun.security.util.Length
import y2023.Number

typealias Number = Long

data class MapEntry(val dstStart: Number, val srcStart: Number, val length: Number) {
    fun map(value: Number): Number? = when (val diff = value - srcStart) {
        in 0..length -> dstStart + diff
        else -> null
    }
}

data class MapValue(val entries: List<MapEntry>, val name: String? = null)
class Day5 : Solution {
    override fun solve(input: String): String {
        val inp2 = "seeds: 79 14 55 13\n" +
                "\n" +
                "seed-to-soil map:\n" +
                "50 98 2\n" +
                "52 50 48\n" +
                "\n" +
                "soil-to-fertilizer map:\n" +
                "0 15 37\n" +
                "37 52 2\n" +
                "39 0 15\n" +
                "\n" +
                "fertilizer-to-water map:\n" +
                "49 53 8\n" +
                "0 11 42\n" +
                "42 0 7\n" +
                "57 7 4\n" +
                "\n" +
                "water-to-light map:\n" +
                "88 18 7\n" +
                "18 25 70\n" +
                "\n" +
                "light-to-temperature map:\n" +
                "45 77 23\n" +
                "81 45 19\n" +
                "68 64 13\n" +
                "\n" +
                "temperature-to-humidity map:\n" +
                "0 69 1\n" +
                "1 0 69\n" +
                "\n" +
                "humidity-to-location map:\n" +
                "60 56 37\n" +
                "56 93 4"

        val splitted = input.split("\n\n")
        val seeds = splitted.first().split(":").last().trim().split(" ").map(String::toLong)
        val maps = splitted.drop(1)
            .map { mapStr ->
                val (nameStr, entryLines) = mapStr.split(":")
                val name = nameStr.split(" ").first()
                val entries = entryLines.lines().filter { it.trim().isNotEmpty() }.map { l ->
                    val (dst, src, range) = l.split(" ").map { it.toLong() }
                    MapEntry(dst, src, range)
                }
                MapValue(entries, name)
            }

        val mapEntries = maps
            .map { map -> map.entries.map { it.srcStart to (it.dstStart to it.length) } }
            .map { sortedMapOf(*it.toTypedArray()) }

        fun findValOfSeed(seed: Number): Number = mapEntries
            .fold(seed) { a, c ->
                val headMap = c.headMap(a + 1)
                if (headMap.isEmpty())
                    return@fold a
                val srcStart = headMap.lastKey()
                val (dstStart, length) = c[srcStart]!!
                if (a - srcStart <= length)
                    a - srcStart + dstStart
                else a
            }

        val p1 = seeds.minOf(::findValOfSeed)
        val seedRanges = seeds.windowed(2, 2)
            .map { r -> r.first()..<r.first() + r.last() }
            .sortedBy { it.first }

        fun getImportantLocations(maps: List<MapValue>): List<Long> {
            val reversed = maps.asReversed()
            val initial = reversed.first().entries.map { it.srcStart }.sorted()
                .let { if (it.first() == 0L) it else listOf(0L) + it }

            return reversed.fold(initial) { acc, curr ->
                val dstMap = curr.entries.map { it.dstStart to (it.srcStart to it.length) }
                    .let { sortedMapOf(*it.toTypedArray()) }
                // map the acc to "points" in curr src space
                val mapped = acc.map {
                    val headMap = dstMap.headMap(it + 1)
                    if (headMap.isEmpty())
                        it
                    else {
                        val key = headMap.lastKey()
                        val found = dstMap[key]!!
                        if (it - key < found.second)
                            it - key + found.first
                        else
                            it
                    }
                }
                // return mapped points and source points of current (and 0 if not included)
                val currPts = curr.entries.map { it.srcStart }.sorted()
                    .let { if (it.first() == 0L) it else listOf(0L) + it }
                (mapped + currPts).toSet().toList()
            }.sorted()
        }

        val importantLocations = getImportantLocations(maps)
        val locationsInRanges = importantLocations.filter { loc -> seedRanges.any { it.contains(loc) } }

        println("size of important locations ${importantLocations.size}\nsize of locations in ranges ${locationsInRanges.size}")
        val p2 = locationsInRanges.minOf(::findValOfSeed)

        return "Part1: $p1\nPart2: $p2"
    }
}
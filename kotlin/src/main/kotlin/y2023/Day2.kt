package y2023

import Solution
import y2023.CubeColors.*

enum class CubeColors {
    RED, BLUE, GREEN
}

data class GameSet(val cubes: Map<CubeColors, Int>)

data class Game(val id: Int, val sets: List<GameSet>)
class Day2: Solution {
    override fun solve(input: String): String {
        val inp2 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n" +
                "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n" +
                "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n" +
                "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n" +
                "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        val gameSets = input.lines()
            .map { line ->
                val splitted = line.split(":")
                val gameId = splitted.first().split(" ").last().toInt()
                val sets = splitted.last().split(";")
                    .map { setStr -> setStr.split(",")
                        .map {
                            val splittedSet = it.trim().split(" ")
                            val color = CubeColors.valueOf(splittedSet.last().uppercase())
                            val count = splittedSet.first().trim().toInt()
                            mapOf(color to count)
                        }.fold(emptyMap<CubeColors, Int>()) {l, e-> l + e}
                    }.map { GameSet(it) }
                Game(gameId, sets)
            }


        val maxP1 = mapOf(RED to 12, GREEN to 13, BLUE to 14)
        val p1 = gameSets.filter { gs -> gs.sets
            .none { set -> maxP1.any { e -> (set.cubes[e.key] ?: 0) > e.value } }
        }.sumOf { it.id }

        val powers = gameSets.map { gs -> gs.sets
            .map(GameSet::cubes)
            .reduce { a, c -> CubeColors.entries.map { it to (a[it] ?: 0)
                .coerceAtLeast(c[it] ?: 0)
            }.toMap()}
            .values.reduce { a, b -> a * b}
        }
        val p2 = powers.sum()
        return "Part1: $p1\nPart2: $p2"
    }
}
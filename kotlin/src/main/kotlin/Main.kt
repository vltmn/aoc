import java.io.File
import java.nio.charset.Charset
import java.nio.file.Paths

fun main() {
    val solution: Solution = y2023.Day2()
    val day = solution.javaClass.simpleName.substring(3).toInt()
    val year = solution.javaClass.`package`.name
        .split("\\.").last().substring(1)
    val inputPath = Paths.get("src", "main", "resources", "y$year", "day$day")
    val input = File(inputPath.toUri()).inputStream().readBytes().toString(Charset.defaultCharset())

    val output = solution.solve(input)

    println("Output was:\n$output")
}
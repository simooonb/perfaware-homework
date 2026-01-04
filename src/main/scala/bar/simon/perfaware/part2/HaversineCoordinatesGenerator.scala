package bar.simon.perfaware.part2

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.Random

object HaversineCoordinatesGenerator {
  val earthRadius: Double = 6372.8

  def generate(generationMode: String, seed: Long, pairsAmount: Int): Double = {
    Random.setSeed(seed)

    val (avg: Double, json: String) =
      if (generationMode == "uniform")
        generateUniform(pairsAmount)
      else if (generationMode == "cluster")
        generateClustered(pairsAmount)
      else {
        println(s"Unknown generation mode $generationMode")
        (-1, "")
      }

    Files.write(Paths.get("src/main/resources/input.json"), json.getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("src/main/resources/expected.f64"), avg.toString.getBytes(StandardCharsets.UTF_8))

    avg
  }

  private def generateUniform(pairsAmount: Int): (Double, String) = {
    var avg: Double       = 0d
    val sb: StringBuilder = new StringBuilder()
    val avgCoef: Double   = 1.0 / pairsAmount

    sb.append("{\"pairs\": [")

    (0 until pairsAmount).foreach { i =>
      val x0 = -180 + Random.nextDouble() * 360
      val y0 = -90 + Random.nextDouble() * 180
      val x1 = -180 + Random.nextDouble() * 360
      val y1 = -90 + Random.nextDouble() * 180

      avg += HaversineDistance.compute(x0, y0, x1, y1, earthRadius) * avgCoef
      sb.append(s"{\"x0\":$x0, \"y0\":$y0, \"x1\":$x1, \"y1\":$y1},\n")
    }

    sb.append("]}")

    (avg, sb.result())
  }

  private def generateClustered(pairsAmount: Int): (Double, String) = {
    var avg: Double       = 0d
    val sb: StringBuilder = new StringBuilder()
    val avgCoef: Double   = 1.0 / pairsAmount

    val clusterAmount  = 64
    val clusterRadiusX = 5
    val clusterRadiusY = 10
    val clusters       = Array.fill(clusterAmount) {
      val x = -180 + Random.nextDouble() * 360
      val y = -90 + Random.nextDouble() * 180
      (x, y)
    }

    sb.append("{\"pairs\": [")

    (0 until pairsAmount).foreach { i =>
      val (centerX, centerY) = clusters(i % clusterAmount)

      val x0 = centerX - clusterRadiusX + Random.nextDouble() * (2 * clusterRadiusX)
      val y0 = centerY - clusterRadiusY + Random.nextDouble() * (2 * clusterRadiusY)
      val x1 = centerX - clusterRadiusX + Random.nextDouble() * (2 * clusterRadiusX)
      val y1 = centerY - clusterRadiusY + Random.nextDouble() * (2 * clusterRadiusY)

      avg += HaversineDistance.compute(x0, y0, x1, y1, earthRadius) * avgCoef
      sb.append(s"{\"x0\":$x0, \"y0\":$y0, \"x1\":$x1, \"y1\":$y1},\n")
    }

    sb.append("]}")

    (avg, sb.result())
  }
}

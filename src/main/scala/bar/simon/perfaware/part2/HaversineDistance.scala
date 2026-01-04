package bar.simon.perfaware.part2

import scala.math._

object HaversineDistance {
  def compute(x0: Double, y0: Double, x1: Double, y1: Double, earthRadius: Double): Double = {
    val lat1 = y0
    val lat2 = y1
    val lon1 = x0
    val lon2 = x1

    val deltaLat = toRadians(lat2 - lat1)
    val deltaLon = toRadians(lon2 - lon1)

    val a = pow(sin(deltaLat / 2), 2) + cos(toRadians(lat1)) * cos(toRadians(lat2)) * pow(sin(deltaLon / 2), 2)
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))

    earthRadius * c
  }
}

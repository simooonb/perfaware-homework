package bar.simon.perfaware.part2

import java.time.Instant

object GeneratorMain extends App {
  private val Array(generationMode, seed, pairs) = args

  val start  = Instant.now()
  val result = HaversineCoordinatesGenerator.generate(generationMode, seed.toLong, pairs.toInt)
  val end    = Instant.now()

  val elapsed = (end.toEpochMilli - start.toEpochMilli) / 1000d

  println(s"Method: $generationMode")
  println(s"Random seed: $seed")
  println(s"Pair count: $pairs")
  println(s"Expected sum: $result")
  println(f"Elapsed time: $elapsed%.3f seconds")
}

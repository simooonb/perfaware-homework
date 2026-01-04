package bar.simon.perfaware.part2

import java.time.Instant
import scala.io.Source

object ProcessorMain extends App {
  private val Array(inputName, expectedName) = args

  val start  = Instant.now()
  val result = HaversineDistanceProcessor.process(inputName)
  val end    = Instant.now()

  val elapsed = (end.toEpochMilli - start.toEpochMilli) / 1000d

  val expected = Source.fromResource(expectedName).getLines().next().toDouble

  println(s"Result: $result")
  println(s"Expected sum: $expected")
  println(s"Difference: ${result - expected}")
  println(f"Elapsed time: $elapsed%.3f seconds")
}

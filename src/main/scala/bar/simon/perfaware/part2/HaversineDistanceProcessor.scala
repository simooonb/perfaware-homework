package bar.simon.perfaware.part2

import bar.simon.perfaware.part2.HaversineCoordinatesGenerator.earthRadius

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object HaversineDistanceProcessor {
  def process(filePath: String): Double = {
    val startTimer: Long = CPUTimer.getCycles

    val (pairs, fileReadTimer, miscTimer): (List[PointPair], Long, Long) = read(filePath)

    val parseTimer: Long = CPUTimer.getCycles
    val pairSize: Int    = pairs.size
    val avgCoef: Double  = 1.0 / pairSize

    val res = pairs.foldLeft(0d) { case (acc, next) =>
      acc + (HaversineDistance.compute(next.x0, next.y0, next.x1, next.y1, earthRadius) * avgCoef)
    }

    val sumTimer: Long      = CPUTimer.getCycles
    val totalTime: Long     = sumTimer - startTimer
    val readFileTime: Long  = fileReadTimer - startTimer
    val miscSetupTime: Long = miscTimer - fileReadTimer
    val parseTime: Long     = parseTimer - miscTimer
    val sumTime: Long       = sumTimer - parseTimer
    val cpuFreq: Long       = 24000000 // todo: get from ASM / guess

    println(f"""File read: $readFileTime (${100.0 * readFileTime / totalTime}%.2f%%)
    |Vars setup: $miscSetupTime (${100.0 * miscSetupTime / totalTime}%.2f%%)
    |Parsing: $parseTime (${100.0 * parseTime / totalTime}%.2f%%)
    |Sum: $sumTime (${100.0 * sumTime / totalTime}%.2f%%)
    |Total: $totalTime
    |Total time: ${totalTime.toDouble / cpuFreq}%.3fs (CPU Freq $cpuFreq)""".stripMargin)

    res
  }

  def read(path: String): (List[PointPair], Long, Long) = {
    val lines: Iterator[String] = Source.fromResource(path).getLines()
    val fileReadTimer: Long     = CPUTimer.getCycles

    val pairList: ArrayBuffer[PointPair] = ArrayBuffer.empty
    val ignored: Set[Char]               = Set(' ', '\n')

    val globalStart   = '['
    val globalEnd     = ']'
    val objectStart   = '{'
    val objectEnd     = '}'
    val delimiter     = ','
    var foundStart    = false
    var foundEnd      = false
    var parsingObject = false
    var buffer        = ArrayBuffer.empty[Char]

    val miscTimer: Long = CPUTimer.getCycles

    while (lines.hasNext && !foundEnd) {
      val line   = lines.next()
      val length = line.length
      var idx    = 0

      while (idx < length) {
        val current = line(idx)

        if (!ignored(current)) {
          if (!foundStart && current == globalStart)
            foundStart = true
          else if (!foundEnd && current == globalEnd)
            foundEnd = true
          else if (foundStart && !foundEnd) {
            if (!parsingObject && current == delimiter) {
              ()
            } else if (!parsingObject && current == objectStart) {
              parsingObject = true
            } else if (parsingObject && current == objectEnd) {
              pairList.addOne(parseBuffer(buffer))
              parsingObject = false
              buffer = ArrayBuffer.empty
            } else {
              buffer.addOne(current)
            }
          }
        }

        idx += 1
      }
    }

    (pairList.toList, fileReadTimer, miscTimer)
  }

  def parseBuffer(buffer: ArrayBuffer[Char]): PointPair = {
    val sb = new StringBuilder()
    for (c <- buffer)
      sb.append(c)
    val result           = sb.result()
    val fields           = result.split(",")
    var (x0, y0, x1, y1) = (0d, 0d, 0d, 0d)

    fields.foreach { fieldStr =>
      val Array(fieldName, tail) = fieldStr.split('\"').filter(_.nonEmpty)
      val value                  = tail.tail.toDouble
      fieldName match {
        case "x0" => x0 = value
        case "y0" => y0 = value
        case "x1" => x1 = value
        case "y1" => y1 = value
      }
    }

    PointPair(x0, y0, x1, y1)
  }
}

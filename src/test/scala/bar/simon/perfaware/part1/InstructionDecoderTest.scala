package bar.simon.perfaware.part1

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class InstructionDecoderTest extends AnyWordSpec with Matchers {

  "Instruction decoder" should {
    "decode a single MOV instruction" in {
      val input = readResourceAsBytes("hw1/listing_0037_single_register_mov")
      val expected = readResourceAsString("hw1/listing_0037_single_register_mov.asm")

      InstructionDecoder.decodeAll(input).mkString("\n") shouldBe expected
    }

    "decode many MOV instructions" in {
      val input = readResourceAsBytes("hw1/listing_0038_many_register_mov")
      val expected = readResourceAsString("hw1/listing_0038_many_register_mov.asm")

      InstructionDecoder.decodeAll(input).mkString("\n") shouldBe expected
    }
  }

  private def readResourceAsBytes(filename: String): Array[Byte] =
    Files.readAllBytes(Path.of(getClass.getResource(filename).toURI))

  private def readResourceAsString(filename: String): String =
    Files.readString(Path.of(getClass.getResource(filename).toURI))
}

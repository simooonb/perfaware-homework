package bar.simon.perfaware.part1

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class InstructionDecoderTest extends AnyWordSpec with Matchers {

  "Instruction decoder" should {
    "decode a single MOV instruction" in {
      val input    = readResourceAsBytes("hw1/listing_0037_single_register_mov")
      val expected = readResourceAsString("hw1/listing_0037_single_register_mov.asm")

      InstructionDecoder.decodeAll(input).mkString("\n") shouldBe expected
    }

    "decode many MOV instructions" in {
      val input    = readResourceAsBytes("hw1/listing_0038_many_register_mov")
      val expected = readResourceAsString("hw1/listing_0038_many_register_mov.asm")

      InstructionDecoder.decodeAll(input).mkString("\n") shouldBe expected
    }

    "decode many MOV instructions with effective address calculation" in {
      val input    = readResourceAsBytes("hw2/listing_0039_more_movs")
      val expected = readResourceAsString("hw2/listing_0039_more_movs.asm")

      InstructionDecoder.decodeAll(input).mkString("\n") shouldBe expected
    }

    // fixme: does not pass because of some int decoding instead of unsigned int decoding
    "decode ADD, SUB, CMP, JNZ" in {
      val input    = readResourceAsBytes("hw3/listing_0041_add_sub_cmp_jnz")
      val expected = readResourceAsString("hw3/listing_0041_add_sub_cmp_jnz.asm")

      InstructionDecoder.decodeAll(input).mkString("\n") shouldBe expected
    }
  }

  "Register simulation" should {
    "decode immediate MOVs" in {
      val input    = readResourceAsBytes("hw4/listing_0043_immediate_movs")
      val expected = readResourceAsString("hw4/listing_0043_immediate_movs.asm")

      InstructionDecoder.decodeAll(input).mkString("\n") shouldBe expected
    }

    "decode register MOVs" in {
      val input    = readResourceAsBytes("hw4/listing_0044_register_movs")
      val expected = readResourceAsString("hw4/listing_0044_register_movs.asm")

      InstructionDecoder.decodeAll(input).mkString("\n") shouldBe expected
    }
  }

  private def readResourceAsBytes(filename: String): Array[Byte] =
    Files.readAllBytes(Path.of(getClass.getResource(filename).toURI))

  private def readResourceAsString(filename: String): String =
    Files.readString(Path.of(getClass.getResource(filename).toURI))
}

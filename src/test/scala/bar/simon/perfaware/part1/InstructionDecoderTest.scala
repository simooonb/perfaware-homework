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

    "simulate immediate MOVs" in {
      val input = readResourceAsBytes("hw4/listing_0043_immediate_movs")
      // idk why it's read weirdly
//      val expected = readResourceAsString("hw4/listing_0043_immediate_movs.txt")

      val expected =
        """mov ax, 1 ; ax:0x0->0x1
          |mov bx, 2 ; bx:0x0->0x2
          |mov cx, 3 ; cx:0x0->0x3
          |mov dx, 4 ; dx:0x0->0x4
          |mov sp, 5 ; sp:0x0->0x5
          |mov bp, 6 ; bp:0x0->0x6
          |mov si, 7 ; si:0x0->0x7
          |mov di, 8 ; di:0x0->0x8
          |
          |Final registers:
          |      ax: 0x0001 (1)
          |      bx: 0x0002 (2)
          |      cx: 0x0003 (3)
          |      dx: 0x0004 (4)
          |      sp: 0x0005 (5)
          |      bp: 0x0006 (6)
          |      si: 0x0007 (7)
          |      di: 0x0008 (8)""".stripMargin

      RegisterSimulation.reset()
      RegisterSimulation.simulateFromAsm(InstructionDecoder.decodeAll(input)) shouldBe expected
    }

    "simulate register MOVs" in {
      val input = readResourceAsBytes("hw4/listing_0044_register_movs")
      // idk why it's read weirdly
      //      val expected = readResourceAsString("hw4/listing_0044_register_movs.txt")

      val expected =
        """mov ax, 1 ; ax:0x0->0x1
          |mov bx, 2 ; bx:0x0->0x2
          |mov cx, 3 ; cx:0x0->0x3
          |mov dx, 4 ; dx:0x0->0x4
          |mov sp, ax ; sp:0x0->0x1
          |mov bp, bx ; bp:0x0->0x2
          |mov si, cx ; si:0x0->0x3
          |mov di, dx ; di:0x0->0x4
          |mov dx, sp ; dx:0x4->0x1
          |mov cx, bp ; cx:0x3->0x2
          |mov bx, si ; bx:0x2->0x3
          |mov ax, di ; ax:0x1->0x4
          |
          |Final registers:
          |      ax: 0x0004 (4)
          |      bx: 0x0003 (3)
          |      cx: 0x0002 (2)
          |      dx: 0x0001 (1)
          |      sp: 0x0001 (1)
          |      bp: 0x0002 (2)
          |      si: 0x0003 (3)
          |      di: 0x0004 (4)""".stripMargin

      RegisterSimulation.reset()
      RegisterSimulation.simulateFromAsm(InstructionDecoder.decodeAll(input)) shouldBe expected
    }
  }

  private def readResourceAsBytes(filename: String): Array[Byte] =
    Files.readAllBytes(Path.of(getClass.getResource(filename).toURI))

  private def readResourceAsString(filename: String): String =
    Files.readString(Path.of(getClass.getResource(filename).toURI))
}

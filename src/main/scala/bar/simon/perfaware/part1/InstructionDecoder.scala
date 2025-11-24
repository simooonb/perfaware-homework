package bar.simon.perfaware.part1

import bar.simon.perfaware.part1.Instruction._

import scala.annotation.tailrec

object InstructionDecoder {
  def decodeAll(input: Array[Byte]): Array[String] = {
    val output = Array.ofDim[String](2)
    output(0) = "bits 16"
    output(1) = ""

    val result = readByGroup(0, input, Nil)

    output ++ result.toArray
  }

  @tailrec
  private def readByGroup(i: Int, input: Array[Byte], acc: List[String]): List[String] = {
    if (i >= input.length) {
      acc.reverse
    } else {
      val current = input(i)

      if (current.isJump) {
        val result = decodeJump(jumpLookupTable(current.toBinaryString), input(i + 1))

        readByGroup(i + 2, input, result :: acc)
      } else if (
        current.isMovFromToMemoryOrRegister ||
        current.isAddFromToMemoryOrRegister ||
        current.isSubFromToMemoryOrRegister ||
        current.isCmpFromToMemoryOrRegister
      ) {
        val instruction =
          if (current.isMovFromToMemoryOrRegister) Mov
          else if (current.isAddFromToMemoryOrRegister) Add
          else if (current.isCmpFromToMemoryOrRegister) Cmp
          else Sub

        val (nextIndex, result) = decodeFromToMemoryOrRegister(input, i, current, instruction)

        readByGroup(nextIndex, input, result :: acc)
      } else if (
        current.isMovImmediateToRegister ||
        current.isAddImmediateToAccumulator ||
        current.isSubImmediateFromAccumulator ||
        current.isCmpImmediateWithAccumulator
      ) {
        val instruction =
          if (current.isMovImmediateToRegister) Mov
          else if (current.isAddImmediateToAccumulator) Add
          else if (current.isCmpImmediateWithAccumulator) Cmp
          else Sub

        val (result, next) = decodeImmediateToRegister(input, i, current, instruction)

        readByGroup(next, input, result :: acc)
      } else if (
        current.isMovImmediateToRegisterMemory ||
        current.isAddImmediateToRegisterMemory ||
        current.isSubImmediateFromRegisterMemory ||
        current.isCmpImmediateFromRegisterMemory
      ) {
        val (nextIndex, result) = decodeImmediateToRegisterMemory(input, i, current)

        readByGroup(nextIndex, input, result :: acc)
      } else {
        readByGroup(i + 1, input, "unknown operation" :: acc)
      }
    }
  }

  private def decodeJump(opcode: String, next: Byte): String =
    s"$opcode ${next.toInt}"

  private def decodeImmediateToRegisterMemory(
      input: Array[Byte],
      i: Int,
      current: Byte
  ): (Int, String) = {
    val next = input(i + 1)

    val instruction =
      if (current.isMovImmediateToRegisterMemory) Mov
      else if (next.isSubOpCodeImmediateFromRegisterMemory) Sub
      else if (next.isCmpOpCodeImmediateFromRegisterMemory) Cmp
      else Add

    val wide                      = current.isImmediateToRegisterMemoryWide
    val signed                    = current.isAddSigned
    val effectiveAddress          = next.isModEffectiveAddressCalculation
    val address8BitsDisplacement  = next.isModEffectiveAddressCalculation8BitsDisplacement
    val address16BitsDisplacement = next.isModEffectiveAddressCalculation16BitsDisplacement

    val displacements =
      if (address16BitsDisplacement) Array(input(i + 2), input(i + 3))
      else if (address8BitsDisplacement) Array(input(i + 2))
      else if (effectiveAddress) Array.empty[Byte]
      else Array.empty[Byte]

    val immediate =
      if (address8BitsDisplacement && wide && !signed && instruction.isNotMov) Array(input(i + 3), input(i + 4))
      else if (address8BitsDisplacement && wide && instruction == Mov) Array(input(i + 3), input(i + 4))
      else if (address8BitsDisplacement) Array(input(i + 3))
      else if (address16BitsDisplacement && wide && !signed && instruction.isNotMov) Array(input(i + 4), input(i + 5))
      else if (address16BitsDisplacement && wide && instruction == Mov) Array(input(i + 4), input(i + 5))
      else if (address16BitsDisplacement) Array(input(i + 4))
      else if (effectiveAddress && wide && !signed && instruction.isNotMov) Array(input(i + 2), input(i + 3))
      else if (effectiveAddress && wide && instruction == Mov) Array(input(i + 2), input(i + 3))
      else if (effectiveAddress) Array(input(i + 2))
      else if (wide && !signed && instruction.isNotMov) Array(input(i + 2), input(i + 3))
      else if (wide && instruction == Mov) Array(input(i + 2), input(i + 3))
      else Array(input(i + 2))

    val nextIndex =
      if (address8BitsDisplacement && wide && !signed && instruction.isNotMov) i + 5
      else if (address8BitsDisplacement && wide && instruction == Mov) i + 5
      else if (address8BitsDisplacement) i + 4
      else if (address16BitsDisplacement && wide && !signed && instruction.isNotMov) i + 6
      else if (address16BitsDisplacement && wide && instruction == Mov) i + 6
      else if (address16BitsDisplacement) i + 5
      else if (effectiveAddress && wide && !signed && instruction.isNotMov) i + 4
      else if (effectiveAddress && wide && instruction == Mov) i + 4
      else if (effectiveAddress) i + 3
      else if (wide && !signed && instruction.isNotMov) i + 4
      else if (wide && instruction == Mov) i + 4
      else i + 3

    val result = decodeImmediateToRegisterMemory(next, wide, displacements, immediate, instruction)

    (nextIndex, result)
  }

  private def decodeImmediateToRegister(
      input: Array[Byte],
      i: Int,
      current: Byte,
      instruction: Instruction
  ): (String, Int) =
    if (current.isImmediateToRegisterWide)
      decodeImmediateToRegister(current, Array(input(i + 1), input(i + 2)), instruction) -> (i + 3)
    else
      decodeImmediateToRegister(current, Array(input(i + 1)), instruction) -> (i + 2)

  private def decodeFromToMemoryOrRegister(
      input: Array[Byte],
      i: Int,
      current: Byte,
      instruction: Instruction
  ): (Int, String) = {
    val next = input(i + 1)

    val regToReg                  = next.isModRegToReg
    val effectiveAddress          = next.isModEffectiveAddressCalculation
    val address8BitsDisplacement  = next.isModEffectiveAddressCalculation8BitsDisplacement
    val address16BitsDisplacement = next.isModEffectiveAddressCalculation16BitsDisplacement

    val displacements =
      if (effectiveAddress) Array.empty[Byte]
      else if (address8BitsDisplacement) Array(input(i + 2))
      else if (address16BitsDisplacement) Array(input(i + 2), input(i + 3))
      else Array.empty[Byte]

    val nextIndex =
      if (regToReg || effectiveAddress) i + 2
      else if (address8BitsDisplacement) i + 3
      else if (address16BitsDisplacement) i + 4
      else i + 2

    val result =
      if (regToReg) decodeRegToReg(current, next, instruction)
      else decodeEffectiveAddressCalculation(current, next, displacements, instruction)

    (nextIndex, result)
  }

  def decodeImmediateToRegister(b: Byte, data: Array[Byte], instruction: Instruction): String = {
    val reg       = (b << 5) >> 5
    val wide      = b.isImmediateToRegisterWide
    val immediate = BigInt(1, data.reverse).toInt
    val register =
      if (instruction.isNotMov && wide) "ax"
      else if (instruction.isNotMov) "al"
      else movRegisterDecode(reg, wide)

    s"${instruction.opcode} $register, $immediate"
  }

  def decodeImmediateToRegisterMemory(
      b: Byte,
      wide: Boolean,
      displacements: Array[Byte],
      data: Array[Byte],
      instruction: Instruction
  ): String = {
    val rm           = b & (0xff >>> 5)
    val mod          = (b & (3 << 6)) >>> 6
    val immediate    = BigInt(1, data.reverse).toInt
    val displacement = if (displacements.nonEmpty) BigInt(1, displacements).toInt else 0

    val register =
      if (mod.toBinaryString == "11") {
        movRegisterDecode(rm, wide)
      } else {
        val cast = if (wide) "word" else "byte"
        s"$cast ${effectiveAddressDecode(mod, rm, displacement)}"
      }

    s"${instruction.opcode} $register, $immediate"
  }

  def decodeEffectiveAddressCalculation(
      b1: Byte,
      b2: Byte,
      displacements: Array[Byte],
      instruction: Instruction
  ): String = {
    val mod              = (b2 & (3 << 6)) >>> 6
    val reg              = (b2 >>> 3) & ~(1 << 3)
    val rm               = b2 & (0xff >>> 5)
    val wide             = b1.toBinaryString.last == '1'
    val regIsDestination = (b1 & 2) >>> 1 == 1 // 1 REG is destination, 0 REG is source

    val displacement = if (displacements.nonEmpty) BigInt(1, displacements).toInt else 0
    val rmRegister   = effectiveAddressDecode(mod, rm, displacement)
    val regRegister  = movRegisterDecode(reg, wide)
    val opcode       = instruction.opcode

    if (regIsDestination)
      s"$opcode $regRegister, $rmRegister"
    else
      s"$opcode $rmRegister, $regRegister"
  }

  def decodeRegToReg(b1: Byte, b2: Byte, instruction: Instruction): String = {
    val regIsDestination = (b1 & 2) >>> 1 == 1           // 1 REG is destination, 0 REG is source
    val wide             = b1.toBinaryString.last == '1' // 1 4 bits, 0 8 bits

    val reg = (b2 >> 3) & ~(1 << 3)
    val rm  = b2 & ~(1 << 3)

    val regRegister = movRegisterDecode(reg, wide)
    val rmRegister  = movRegisterDecode(rm, wide)
    val opcode      = instruction.opcode

    if (regIsDestination)
      s"$opcode $regRegister, $rmRegister"
    else
      s"$opcode $rmRegister, $regRegister"
  }

  def effectiveAddressDecode(mod: Int, reg: Int, displacement: Int): String =
    reg.toBinaryString match {
      case "0" if mod == 0 || displacement == 0   => s"[bx + si]"
      case "0"                                    => s"[bx + si + $displacement]"
      case "1" if mod == 0 || displacement == 0   => s"[bx + di]"
      case "1"                                    => s"[bx + di + $displacement]"
      case "10" if mod == 0 || displacement == 0  => s"[bp + si]"
      case "10"                                   => s"[bp + si + $displacement]"
      case "11" if mod == 0 || displacement == 0  => s"[bp + di]"
      case "11"                                   => s"[bp + di + $displacement]"
      case "100" if mod == 0 || displacement == 0 => s"[si]"
      case "100"                                  => s"[si + $displacement]"
      case "101" if mod == 0 || displacement == 0 => s"[di]"
      case "101"                                  => s"[di + $displacement]"
      case "110" if mod == 0                      => s"[$displacement]"
      case "110" if displacement == 0             => s"[bp]"
      case "110"                                  => s"[bp + $displacement]"
      case "111" if mod == 0 || displacement == 0 => s"[bx]"
      case "111"                                  => s"[bx + $displacement]"
    }

  def movRegisterDecode(reg: Int, wide: Boolean): String =
    reg.toBinaryString.takeRight(3) match {
      case "000" if wide => "ax"
      case "0" if wide   => "ax"
      case "000"         => "al"
      case "0"           => "al"
      case "001" if wide => "cx"
      case "1" if wide   => "cx"
      case "001"         => "cl"
      case "1"           => "cl"
      case "010" if wide => "dx"
      case "10" if wide  => "dx"
      case "010"         => "dl"
      case "10"          => "dl"
      case "011" if wide => "bx"
      case "11" if wide  => "bx"
      case "011"         => "bl"
      case "11"          => "bl"
      case "100" if wide => "sp"
      case "100"         => "ah"
      case "101" if wide => "bp"
      case "101"         => "ch"
      case "110" if wide => "si"
      case "110"         => "dh"
      case "111" if wide => "di"
      case "111"         => "bh"
    }

  implicit class IntOps(i: Int) {
    def toBinaryString16Bits: String =
      f"${(i & 0xffff).toBinaryString}%16s".replace(' ', '0')
  }

  implicit class ByteOps(byte: Byte) {
    def toBinaryString: String =
      f"${(byte & 0xff).toBinaryString}%8s".replace(' ', '0')

    def isJump: Boolean =
      jumpLookupTable.contains(byte.toBinaryString)

    def isMovFromToMemoryOrRegister: Boolean =
      byte >> 2 == -30 // "100010"

    def isAddFromToMemoryOrRegister: Boolean =
      byte >>> 2 == 0

    def isSubFromToMemoryOrRegister: Boolean =
      (byte >>> 2 & 0xff).toBinaryString == "1010" // "001010"

    def isCmpFromToMemoryOrRegister: Boolean =
      (byte >>> 2 & 0xff).toBinaryString == "1110" // "001110"

    def isMovImmediateToRegister: Boolean =
      byte >> 4 == -5 // "1011"

    def isAddImmediateToAccumulator: Boolean =
      byte >>> 1 == 2 // "0000010"

    def isSubImmediateFromAccumulator: Boolean =
      (byte >>> 1 & 0xff).toBinaryString == "10110" // "0010110"

    def isCmpImmediateWithAccumulator: Boolean =
      (byte >>> 1 & 0xff).toBinaryString == "11110" // "0011110"

    def isImmediateToRegisterWide: Boolean =
      ((byte >>> 3) & 1) == 1

    def isMovImmediateToRegisterMemory: Boolean =
      byte >> 1 == -29 // "1100011"

    def isAddImmediateToRegisterMemory: Boolean =
      ((byte & 0xff) >>> 2).toBinaryString == "100000"

    def isSubImmediateFromRegisterMemory: Boolean =
      ((byte & 0xff) >>> 2).toBinaryString == "100000"

    def isSubOpCodeImmediateFromRegisterMemory: Boolean =
      ((byte & (7 << 3)) >>> 3).toBinaryString == "101"

    def isCmpImmediateFromRegisterMemory: Boolean =
      ((byte & 0xff) >>> 2).toBinaryString == "100000"

    def isCmpOpCodeImmediateFromRegisterMemory: Boolean =
      ((byte & (7 << 3)) >>> 3).toBinaryString == "111"

    def isImmediateToRegisterMemoryWide: Boolean =
      byte.toBinaryString.last == '1'

    def isAddSigned: Boolean =
      byte.toBinaryString(6) == '1'

    def isModRegToReg: Boolean =
      byte >> 6 == -1 // "11"

    def isModEffectiveAddressCalculation: Boolean =
      byte >> 6 == 0 // "00"

    def isModEffectiveAddressCalculation8BitsDisplacement: Boolean =
      byte >> 6 == 1 // "01"

    def isModEffectiveAddressCalculation16BitsDisplacement: Boolean = {
      ((byte & 0xff) >>> 6 == 2) || // "10"
      (((byte & 0xff) >>> 6 == 0) && ((byte & 7) == 6))
    }
  }

  private val jumpLookupTable: Map[String, String] = Map(
    "01110101" -> "jnz",
    "01110100" -> "je",
    "01111100" -> "jl",
    "01111110" -> "jle",
    "01110010" -> "jb",
    "01110110" -> "jbe",
    "01111010" -> "jp",
    "01110000" -> "jo",
    "01111000" -> "js",
    "01110101" -> "jne",
    "01111101" -> "jnl",
    "01111111" -> "jnle",
    "01110011" -> "jnb",
    "01110111" -> "jnbe",
    "01111011" -> "jnp",
    "01110001" -> "jno",
    "01111001" -> "jns",
    "11100010" -> "loop",
    "11100001" -> "loopz",
    "11100000" -> "loopnz",
    "11100011" -> "jcxz"
  )
}

package bar.simon.perfaware.part1

import scala.annotation.tailrec

object InstructionDecoder {
  implicit class ByteOps(byte: Byte) {
    def toBinaryString: String =
      f"${(byte & 0xff).toBinaryString}%8s".replace(' ', '0')

    def isMovFromToMemoryOrRegister: Boolean =
      byte >> 2 == -30 // "100010"

    def isMovImmediateToRegister: Boolean =
      byte >> 4 == -5 // "1011"

    def isMovImmediateToRegisterWide: Boolean =
      byte.toBinaryString.takeRight(8)(4) == '1'

    def isMovImmediateToRegisterMemory: Boolean =
      byte >> 1 == -29 // "1100011"

    def isMovImmediateToRegisterMemoryWide: Boolean =
      byte.toBinaryString.last == '1'

    def isModRegToReg: Boolean =
      byte >> 6 == -1 // "11"

    def isModEffectiveAddressCalculation: Boolean =
      byte >> 6 == 0 // "00"

    def isModEffectiveAddressCalculation8BitsDisplacement: Boolean =
      byte >> 6 == 1 // "01"

    def isModEffectiveAddressCalculation16BitsDisplacement: Boolean =
      (byte & 0xff) >>> 6 == 2 // "10"
  }

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

      if (current.isMovFromToMemoryOrRegister) {
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
          if (regToReg) decodeMovRegToReg(current, next)
          else decodeMovEffectiveAddressCalculation(current, next, displacements)

        readByGroup(nextIndex, input, result :: acc)
      } else if (current.isMovImmediateToRegister) {
        val (result, next) =
          if (!current.isMovImmediateToRegisterWide)
            decodeMovImmediateToRegister(current, Array(input(i + 1))) -> (i + 2)
          else
            decodeMovImmediateToRegister(current, Array(input(i + 1), input(i + 2))) -> (i + 3)

        readByGroup(next, input, result :: acc)
      } else if (current.isMovImmediateToRegisterMemory) {
        val next = input(i + 1)

        val wide                      = current.isMovImmediateToRegisterMemoryWide
        val effectiveAddress          = next.isModEffectiveAddressCalculation
        val address8BitsDisplacement  = next.isModEffectiveAddressCalculation8BitsDisplacement
        val address16BitsDisplacement = next.isModEffectiveAddressCalculation16BitsDisplacement

        val displacements =
          if (effectiveAddress) Array.empty[Byte]
          else if (address8BitsDisplacement) Array(input(i + 2))
          else if (address16BitsDisplacement) Array(input(i + 2), input(i + 3))
          else Array.empty[Byte]

        val immediate =
          if (effectiveAddress && wide) Array(input(i + 2), input(i + 3))
          else if (effectiveAddress) Array(input(i + 2))
          else if (address8BitsDisplacement && wide) Array(input(i + 3), input(i + 4))
          else if (address8BitsDisplacement) Array(input(i + 3))
          else if (address16BitsDisplacement && wide) Array(input(i + 4), input(i + 5))
          else if (address16BitsDisplacement) Array(input(i + 4))
          else Array.empty[Byte] // should not happen

        val nextIndex =
          if (effectiveAddress && wide) i + 4
          else if (effectiveAddress) i + 3
          else if (address8BitsDisplacement && wide) i + 5
          else if (address8BitsDisplacement) i + 4
          else if (address16BitsDisplacement && wide) i + 6
          else if (address16BitsDisplacement) i + 5
          else i + 3 // should not happen

        val result = decodeMovImmediateToRegisterMemory(current, displacements, immediate)

        readByGroup(nextIndex, input, result :: acc)
      } else {
        readByGroup(i + 1, input, "unknown operation" :: acc)
      }
    }
  }

  def decodeMovImmediateToRegister(b: Byte, data: Array[Byte]): String = {
    val reg       = (b << 5) >> 5
    val wide      = b.isMovImmediateToRegisterWide
    val immediate = BigInt(data).toInt

    s"mov ${movRegisterDecode(reg, wide)}, $immediate"
  }

  def decodeMovImmediateToRegisterMemory(b: Byte, displacements: Array[Byte], data: Array[Byte]): String = {
    val rm        = b & ~(1 << 2)
    val wide      = b.isMovImmediateToRegisterWide
    val immediate = BigInt(data).toInt

    s"mov ${movRegisterDecode(rm, wide)}, $immediate"
  }

  def decodeMovEffectiveAddressCalculation(b1: Byte, b2: Byte, displacements: Array[Byte]): String = {
    val mod              = (b2 & (3 << 6)) >>> 6
    val reg              = (b2 >>> 3) & ~(1 << 3)
    val rm               = b2 & (0xff >>> 5)
    val wide             = b1.toBinaryString.last == '1'
    val regIsDestination = (b1 & 2) >>> 1 == 1 // 1 REG is destination, 0 REG is source

    val displacement = if (displacements.nonEmpty) BigInt(displacements).toInt else 0
    val rmRegister   = effectiveAddressDecode(mod, rm, displacement)
    val regRegister  = movRegisterDecode(reg, wide)

    if (regIsDestination)
      s"mov $regRegister, $rmRegister"
    else
      s"mov $rmRegister, $regRegister"
  }

  def decodeMovRegToReg(b1: Byte, b2: Byte): String = {
    val regIsDestination = (b1 & 2) >>> 1 == 1           // 1 REG is destination, 0 REG is source
    val wide             = b1.toBinaryString.last == '1' // 1 4 bits, 0 8 bits

    val reg = (b2 >> 3) & ~(1 << 3)
    val rm  = b2 & ~(1 << 3)

    val regRegister = movRegisterDecode(reg, wide)
    val rmRegister  = movRegisterDecode(rm, wide)

    if (regIsDestination)
      s"mov $regRegister, $rmRegister"
    else
      s"mov $rmRegister, $regRegister"
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
      case "110" if mod == 0                      => s"direct address" // todo
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
}

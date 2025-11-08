package bar.simon.perfaware.part1

object InstructionDecoder {
  def decodeAll(input: Array[Byte]): Array[String] = {
    val output = Array.ofDim[String](2 + input.length / 2)
    output(0) = "bits 16"
    output(1) = ""
    var (i, o) = (0, 2)

    while (i < input.length) {
      val binaryInstruction = Array(input(i), input(i + 1))
      output(o) = InstructionDecoder.decodeSingle(binaryInstruction)

      i += 2
      o += 1
    }

    output
  }

  private def decodeSingle(input: Array[Byte]): String = {
    input match {
      // 100010DW MODREGR/M
      // ^^ -30    2  3  3
      case Array(first, second) if (first >> 2) == -30 =>
        decodeMov(first, second)

      case _ =>
        println("Unknown instructions")
        ""
    }
  }

  def decodeMov(b1: Byte, b2: Byte): String = {
    val regIsDestination = (b1 & 2) == 1 // 1 REG is destination, 0 REG is source
    val wide             = (b1 & 1) == 1 // 1 4 bits, 0 8 bits

    val reg = (b2 << 2) >> 5
    val rm  = (b2 << 5) >> 5

    val regRegister = movRegisterDecode(reg, wide)
    val rmRegister  = movRegisterDecode(rm, wide)

    if (regIsDestination)
      s"mov $regRegister, $rmRegister"
    else
      s"mov $rmRegister, $regRegister"
  }

  def movRegisterDecode(reg: Int, wide: Boolean): String =
    reg.toBinaryString.takeRight(3) match {
      case "000" if wide => "ax"
      case "000"         => "al"
      case "001" if wide => "cx"
      case "001"         => "cl"
      case "010" if wide => "dx"
      case "010"         => "dl"
      case "011" if wide => "bx"
      case "011"         => "bl"
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

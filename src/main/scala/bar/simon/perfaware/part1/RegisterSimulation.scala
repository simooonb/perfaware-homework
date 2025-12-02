package bar.simon.perfaware.part1

import bar.simon.perfaware.part1.Instruction._
import bar.simon.perfaware.part1.InstructionDecoder.IntOps

import java.nio.ByteBuffer

object RegisterSimulation {
  val registers: Array[Char] = Array.fill(9)(0)
  val zero: Flag             = Flag("Z", value = false)
  val sign: Flag             = Flag("S", value = false)

  def reset(): Unit = {
    var i = 0

    while (i < registers.length) {
      registers(i) = 0
      i += 1
    }
  }

  val accessLookup: Map[String, RegisterAccess] = Map(
    "ax" -> RegisterAccess(0, high = true, low = true),
    "bx" -> RegisterAccess(1, high = true, low = true),
    "cx" -> RegisterAccess(2, high = true, low = true),
    "dx" -> RegisterAccess(3, high = true, low = true),
    "ah" -> RegisterAccess(0, high = true, low = false),
    "bh" -> RegisterAccess(1, high = true, low = false),
    "ch" -> RegisterAccess(2, high = true, low = false),
    "dh" -> RegisterAccess(3, high = true, low = false),
    "al" -> RegisterAccess(0, high = false, low = true),
    "bl" -> RegisterAccess(1, high = false, low = true),
    "cl" -> RegisterAccess(2, high = false, low = true),
    "dl" -> RegisterAccess(3, high = false, low = true),
    "sp" -> RegisterAccess(4, high = true, low = true),
    "bp" -> RegisterAccess(5, high = true, low = true),
    "si" -> RegisterAccess(6, high = true, low = true),
    "di" -> RegisterAccess(7, high = true, low = true),
    "ip" -> RegisterAccess(8, high = true, low = true)
  )

  val fullRegisters = Array("ax", "bx", "cx", "dx", "sp", "bp", "si", "di", "ip")

  def ipValue(): Int =
    registers(accessLookup("ip").index)

  def simulateFromAsm(asm: Array[(String, Int)]): String =
    simulate(translate(asm.drop(2)))

  def simulate(instructionApplications: Array[InstructionApplication]): String = {
    var current         = 0
    val instructionsMap = instructionApplications.map { instr =>
      val res = current -> instr
      current += instr.size
      res
    }.toMap

    val result = Array.ofDim[String](1024)
    var i: Int = 0

    while (instructionsMap.contains(ipValue())) {
      val current = instructionsMap(ipValue())
      val ipStr   = if (current.instruction != Instruction.Jne) writeInstructionPointer(current) else ""

      result(i) = current.instruction match {
        case Instruction.Mov     => simulateMov(current, ipStr)
        case Instruction.Add     => simulateAdd(current, ipStr)
        case Instruction.Sub     => simulateSub(current, ipStr)
        case Instruction.Cmp     => simulateCmp(current, ipStr)
        case Instruction.Jne     => simulateJne(current)
        case Instruction.Unknown => "???"
      }

      i += 1
    }

    result(i) = ""
    result(i + 1) = "Final registers:"
    i += 2

    fullRegisters.foreach { reg =>
      val value = registers(accessLookup(reg).index)
      result(i) = f"      $reg: 0x${value.toInt}%04x (${value.toInt})"
      i += 1
    }
    result(i) = s"   flags: ${zero.str}${sign.str}"

    result.filter(_ != null).mkString("\n")
  }

  private def simulateAdd(current: InstructionApplication, ipStr: String): String = {
    val destinationRegister = current.operands(0) match {
      case Operand.Immediate(_)  => "" // should not happen
      case Operand.Register(reg) => reg
    }

    val oldValue = registers(accessLookup(destinationRegister).index)

    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value) => oldValue + value
      case Operand.Register(reg)    => oldValue + registers(accessLookup(reg).index)
    }

    var result  = writeValueInReg(current, valueToWrite.toChar)
    val newZero = valueToWrite == 0
    val newSign = valueToWrite.toBinaryString16Bits.head == '1'

    result += s" $ipStr"

    if (newZero != zero.value || newSign != sign.value) {
      result += " flags:"
      result += zero.setStr(newZero)
      result += sign.setStr(newSign)
    }

    zero.value = newZero
    sign.value = newSign

    result
  }

  private def simulateSub(current: InstructionApplication, ipStr: String): String = {
    val destinationRegister = current.operands(0) match {
      case Operand.Immediate(_)  => "" // should not happen
      case Operand.Register(reg) => reg
    }

    val oldValue = registers(accessLookup(destinationRegister).index)

    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value) => oldValue - value
      case Operand.Register(reg)    => oldValue - registers(accessLookup(reg).index)
    }

    var result  = writeValueInReg(current, valueToWrite.toChar)
    val newZero = valueToWrite == 0
    val newSign = valueToWrite.toBinaryString16Bits.head == '1'

    result += s" $ipStr"

    if (newZero != zero.value || newSign != sign.value) {
      result += " flags:"
      result += zero.setStr(newZero)
      result += sign.setStr(newSign)
    }

    zero.value = newZero
    sign.value = newSign

    result
  }

  private def simulateJne(current: InstructionApplication): String = {
    val offset = current.operands(0) match {
      case Operand.Immediate(value) => (BigInt(ByteBuffer.allocate(2).putChar(value).array()).toInt + 2).toString
      case Operand.Register(value)  => value
    }

    val ipStr = writeInstructionPointer(current)

    s"jne $$$offset ; $ipStr"
  }

  private def simulateCmp(current: InstructionApplication, ipStr: String): String = {
    val destinationRegister = current.operands(0) match {
      case Operand.Immediate(_)  => "" // should not happen
      case Operand.Register(reg) => reg
    }

    val oldValue = registers(accessLookup(destinationRegister).index)

    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value) => oldValue - value
      case Operand.Register(reg)    => oldValue - registers(accessLookup(reg).index)
    }

    val destStr = current.operands(0).str
    var result  = f"${current.instruction.opcode} $destStr, ${current.operands(1).str} ;"
    val newZero = valueToWrite == 0
    val newSign = valueToWrite.toBinaryString16Bits.head == '1'

    result += s" $ipStr"

    if (newZero != zero.value || newSign != sign.value) {
      result += " flags:"
      result += zero.setStr(newZero)
      result += sign.setStr(newSign)
    }

    zero.value = newZero
    sign.value = newSign

    result
  }

  private def simulateMov(current: InstructionApplication, ipStr: String): String = {
    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value) => value
      case Operand.Register(reg)    => registers(accessLookup(reg).index)
    }

    val res = writeValueInReg(current, valueToWrite)

    res + s" $ipStr"
  }

  private def writeValueInReg(current: InstructionApplication, value: Char): String = {
    val destinationRegister = current.operands(0) match {
      case Operand.Immediate(_)  => "" // should not happen
      case Operand.Register(reg) => reg
    }

    val oldValue = registers(accessLookup(destinationRegister).index)
    val access   = accessLookup(destinationRegister)

    val newValue: Char =
      if (access.low && access.high) {
        value
      } else if (access.low) {
        val highValue = (registers(access.index) & 0xff >>> 4) << 4
        val lowValue  = value & 0xff & ~(1 << 4)

        (lowValue & highValue).toChar
      } else if (access.high) {
        val lowValue  = registers(access.index) & 0xff & ~(1 << 4)
        val highValue = (value & 0xff >>> 4) << 4

        (lowValue & highValue).toChar
      } else {
        println("problem")
        0
      }

    val destStr = current.operands(0).str

    registers(access.index) = newValue

    f"${current.instruction.opcode} $destStr, ${current.operands(1).str} ; $destStr:0x${oldValue.toInt}%01x->0x${newValue.toInt}%01x"
  }

  private def writeInstructionPointer(current: InstructionApplication): String = {
    val access         = accessLookup("ip")
    val oldValue: Char = registers(access.index)
    val newValue: Char = current.instruction match {
      case Instruction.Jne if !zero.value =>
        val jump = current.operands(0) match {
          case Operand.Immediate(value) => BigInt(ByteBuffer.allocate(2).putChar(value).array()).toInt + 2
          case Operand.Register(_)      => 0
        }
        (oldValue + jump).toChar

      case _ =>
        (oldValue + current.size).toChar
    }

    registers(access.index) = newValue

    f"ip:0x${oldValue.toInt}%01x->0x${newValue.toInt}%01x"
  }

  def translate(asm: Array[(String, Int)]): Array[InstructionApplication] = {
    val result = Array.ofDim[InstructionApplication](asm.length)
    var i      = 0

    while (i < result.length) {
      val (current, size) = asm(i)

      val operands: Array[Operand] = current.drop(4).split(", ").map { s =>
        s.toIntOption.map(_.toChar) match {
          case Some(value) => Operand.Immediate(value)
          case None        => Operand.Register(s)
        }
      }

      val instruction: Instruction = Instruction.from(current)

      result(i) = InstructionApplication(instruction, operands, size.toChar)

      i += 1
    }

    result
  }
}

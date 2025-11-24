package bar.simon.perfaware.part1

import bar.simon.perfaware.part1.Instruction._
import bar.simon.perfaware.part1.InstructionDecoder.IntOps

object RegisterSimulation {
  val registers: Array[Int] = Array.fill(8)(0)
  val zero: Flag            = Flag("Z", value = false)
  val sign: Flag            = Flag("S", value = false)

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
    "di" -> RegisterAccess(7, high = true, low = true)
  )

  val fullRegisters = Array("ax", "bx", "cx", "dx", "sp", "bp", "si", "di")

  def simulateFromAsm(asm: Array[String]): String =
    simulate(translate(asm.drop(2)))

  def simulate(instructionApplications: Array[InstructionApplication]): String = {
    val result = Array.ofDim[String](instructionApplications.length + 2 + registers.length + 1)
    var i      = 0

    while (i < instructionApplications.length) {
      val current = instructionApplications(i)

      result(i) = current.instruction match {
        case Instruction.Mov     => simulateMov(current)
        case Instruction.Add     => simulateAdd(current)
        case Instruction.Sub     => simulateSub(current)
        case Instruction.Cmp     => simulateCmp(current)
        case Instruction.Unknown => "???"
      }

      i += 1
    }

    result(i) = ""
    result(i + 1) = "Final registers:"
    i += 2

    fullRegisters.foreach { reg =>
      val value = registers(accessLookup(reg).index)
      result(i) = f"      $reg: 0x$value%04x ($value)"
      i += 1
    }
    result(i) = s"   flags: ${zero.str}${sign.str}"

    result.mkString("\n")
  }

  private def simulateAdd(current: InstructionApplication): String = {
    val destinationRegister = current.operands(0) match {
      case Operand.Immediate(_)  => "" // should not happen
      case Operand.Register(reg) => reg
    }

    val oldValue = registers(accessLookup(destinationRegister).index)

    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value) => oldValue + value
      case Operand.Register(reg)    => oldValue + registers(accessLookup(reg).index)
    }

    var result  = writeValueInReg(current, valueToWrite)
    val newZero = valueToWrite == 0
    val newSign = valueToWrite.toBinaryString16Bits.head == '1'

    if (newZero != zero.value || newSign != sign.value) {
      result += " flags:"
      result += zero.setStr(newZero)
      result += sign.setStr(newSign)
    }

    zero.value = newZero
    sign.value = newSign

    result
  }

  private def simulateSub(current: InstructionApplication): String = {
    val destinationRegister = current.operands(0) match {
      case Operand.Immediate(_)  => "" // should not happen
      case Operand.Register(reg) => reg
    }

    val oldValue = registers(accessLookup(destinationRegister).index)

    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value) => oldValue - value
      case Operand.Register(reg)    => oldValue - registers(accessLookup(reg).index)
    }

    var result  = writeValueInReg(current, valueToWrite)
    val newZero = valueToWrite == 0
    val newSign = valueToWrite.toBinaryString16Bits.head == '1'

    if (newZero != zero.value || newSign != sign.value) {
      result += " flags:"
      result += zero.setStr(newZero)
      result += sign.setStr(newSign)
    }

    zero.value = newZero
    sign.value = newSign

    result
  }

  private def simulateCmp(current: InstructionApplication): String = {
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

    if (newZero != zero.value || newSign != sign.value) {
      result += " flags:"
      result += zero.setStr(newZero)
      result += sign.setStr(newSign)
    }

    zero.value = newZero
    sign.value = newSign

    result
  }

  private def simulateMov(current: InstructionApplication): String = {
    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value) => value
      case Operand.Register(reg)    => registers(accessLookup(reg).index)
    }

    writeValueInReg(current, valueToWrite)
  }

  private def writeValueInReg(current: InstructionApplication, value: Int): String = {
    val destinationRegister = current.operands(0) match {
      case Operand.Immediate(_)  => "" // should not happen
      case Operand.Register(reg) => reg
    }

    val oldValue = registers(accessLookup(destinationRegister).index)
    val access   = accessLookup(destinationRegister)

    val newValue: Int =
      if (access.low && access.high) {
        value
      } else if (access.low) {
        val highValue = (registers(access.index) & 0xff >>> 4) << 4
        val lowValue  = value & 0xff & ~(1 << 4)

        lowValue & highValue
      } else if (access.high) {
        val lowValue  = registers(access.index) & 0xff & ~(1 << 4)
        val highValue = (value & 0xff >>> 4) << 4

        lowValue & highValue
      } else {
        println("problem")
        0
      }

    val destStr = current.operands(0).str

    registers(access.index) = newValue

    f"${current.instruction.opcode} $destStr, ${current.operands(1).str} ; $destStr:0x$oldValue%01x->0x$newValue%01x"
  }

  def translate(asm: Array[String]): Array[InstructionApplication] = {
    val result = Array.ofDim[InstructionApplication](asm.length)
    var i      = 0

    while (i < result.length) {
      val current = asm(i)

      val operands: Array[Operand] = current.drop(4).split(", ").map { s =>
        s.toIntOption match {
          case Some(value) => Operand.Immediate(value)
          case None        => Operand.Register(s)
        }
      }

      val instruction: Instruction =
        if (current.startsWith("mov")) Mov
        else if (current.startsWith("add")) Add
        else if (current.startsWith("sub")) Sub
        else if (current.startsWith("cmp")) Cmp
        else Unknown

      result(i) = InstructionApplication(instruction, operands)

      i += 1
    }

    result
  }
}

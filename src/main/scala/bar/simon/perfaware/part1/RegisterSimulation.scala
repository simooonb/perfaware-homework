package bar.simon.perfaware.part1

import bar.simon.perfaware.part1.InstructionDecoder.IntOps

import java.nio.ByteBuffer

object RegisterSimulation {
  val registers: Array[Char] = Array.fill(9)(0)
  val memory: Array[Byte]    = Array.fill(1024 * 1024)(0) // 1 MB

  def getFromMemory(indices: List[Int]): Int =
    indices match {
      case List(i)    => memory(i).toInt
      case List(i, j) => BigInt(1, Array(memory(i), memory(j))).toInt
      case _          => -1
    }

  val zero: Flag = Flag("Z", value = false)
  val sign: Flag = Flag("S", value = false)

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

    if (zero.value || sign.value)
      result(i) = s"   flags: ${zero.str}${sign.str}"

    result.filter(_ != null).mkString("\n")
  }

  private def simulateAdd(current: InstructionApplication, ipStr: String): String = {
    val oldValue = current.operands(0) match {
      case Operand.Immediate(v)                    => v // should not happen
      case Operand.Register(reg)                   => registers(accessLookup(reg).index)
      case Operand.Memory(indices)                 => getFromMemory(indices)
      case Operand.MemoryFromRegister(reg, offset) => getFromMemory(List(registers(accessLookup(reg).index) + offset))
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
    }

    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value)                => oldValue + value
      case Operand.Register(reg)                   => oldValue + registers(accessLookup(reg).index)
      case Operand.Memory(indices)                 => oldValue + getFromMemory(indices)
      case Operand.MemoryFromRegister(reg, offset) =>
        oldValue + getFromMemory(List(registers(accessLookup(reg).index) + offset))
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        oldValue + getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
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
    val oldValue = current.operands(0) match {
      case Operand.Immediate(_)                    => -1.toChar // should not happen
      case Operand.Register(reg)                   => registers(accessLookup(reg).index)
      case Operand.Memory(indices)                 => getFromMemory(indices).toChar
      case Operand.MemoryFromRegister(reg, offset) => getFromMemory(List(registers(accessLookup(reg).index) + offset))
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
    }

    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value)                => oldValue - value
      case Operand.Register(reg)                   => oldValue - registers(accessLookup(reg).index)
      case Operand.Memory(indices)                 => oldValue - getFromMemory(indices).toChar
      case Operand.MemoryFromRegister(reg, offset) =>
        oldValue - getFromMemory(List(registers(accessLookup(reg).index) + offset))
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        oldValue - getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
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
      case Operand.Memory(indices)  => indices.mkString(",")
      case Operand.MemoryFromRegister(reg, offset) => getFromMemory(List(registers(accessLookup(reg).index) + offset))
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
    }

    val ipStr = writeInstructionPointer(current)

    s"jne $$$offset ; $ipStr"
  }

  private def simulateCmp(current: InstructionApplication, ipStr: String): String = {
    val oldValue = current.operands(0) match {
      case Operand.Immediate(_)                    => -1.toChar // should not happen
      case Operand.Register(reg)                   => registers(accessLookup(reg).index)
      case Operand.Memory(indices)                 => getFromMemory(indices)
      case Operand.MemoryFromRegister(reg, offset) => getFromMemory(List(registers(accessLookup(reg).index) + offset))
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
    }

    val valueToWrite = current.operands(1) match {
      case Operand.Immediate(value)                => oldValue - value
      case Operand.Register(reg)                   => oldValue - registers(accessLookup(reg).index)
      case Operand.Memory(indices)                 => oldValue - getFromMemory(indices)
      case Operand.MemoryFromRegister(reg, offset) =>
        oldValue - getFromMemory(List(registers(accessLookup(reg).index) + offset))
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        oldValue - getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
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
      case Operand.Immediate(value)                => value
      case Operand.Register(reg)                   => registers(accessLookup(reg).index)
      case Operand.Memory(indices)                 => getFromMemory(indices).toChar
      case Operand.MemoryFromRegister(reg, offset) =>
        getFromMemory(List(registers(accessLookup(reg).index) + offset)).toChar
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index))).toChar
    }

    val res = writeValueInReg(current, valueToWrite)

    res + s" $ipStr"
  }

  private def writeValueInReg(current: InstructionApplication, value: Char): String = {
    val (oldValue, low, high) = current.operands(0) match {
      case Operand.Immediate(v) =>
        (v, true, true) // should not happen
      case Operand.Register(reg) =>
        val access = accessLookup(reg)
        (registers(access.index), access.low, access.high)
      case Operand.Memory(indices) =>
        (getFromMemory(indices).toChar, true, true)
      case Operand.MemoryFromRegister(reg, offset) =>
        (getFromMemory(List(registers(accessLookup(reg).index) + offset)).toChar, true, true)
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        val res = getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
        (res.toChar, true, true)
    }

    val newValue: Char =
      if (low && high) {
        value
      } else if (low) {
        val highValue = (oldValue & 0xff >>> 4) << 4
        val lowValue  = value & 0xff & ~(1 << 4)

        (lowValue & highValue).toChar
      } else if (high) {
        val lowValue  = oldValue & 0xff & ~(1 << 4)
        val highValue = (value & 0xff >>> 4) << 4

        (lowValue & highValue).toChar
      } else {
        println("problem")
        0
      }

    val destStr = current.operands(0).str

    current.operands(0) match {
      case Operand.Immediate(_) =>
        ()
      case Operand.Register(reg) =>
        registers(accessLookup(reg).index) = newValue
      case Operand.Memory(indices) =>
        indices match {
          case List(i) =>
            memory(i) = newValue.toByte
          case List(i, j) =>
            memory(i) = newValue.toByte
            memory(j) = 0.toByte // todo
          case _ =>
            ()
        }
      case Operand.MemoryFromRegister(reg, offset) =>
        val index = registers(accessLookup(reg).index) + offset
        memory(index) = newValue.toByte
      case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
        val index = registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)
        memory(index) = newValue.toByte
    }

    val regUpdateStr =
      current.operands(0) match {
        case Operand.MemoryFromRegister(_, _)                   => ""
        case Operand.MemoryFromRegisterOffsetFromRegister(_, _) => ""
        case Operand.Memory(_)                                  => ""
        case _ => f" $destStr:0x${oldValue.toInt}%01x->0x${newValue.toInt}%01x"
      }

    f"${current.instruction.opcode} $destStr, ${current.operands(1).str} ;$regUpdateStr"
  }

  private def writeInstructionPointer(current: InstructionApplication): String = {
    val access         = accessLookup("ip")
    val oldValue: Char = registers(access.index)
    val newValue: Char = current.instruction match {
      case Instruction.Jne if !zero.value =>
        val jump = current.operands(0) match {
          case Operand.Immediate(value) => BigInt(ByteBuffer.allocate(2).putChar(value).array()).toInt + 2
          case Operand.Register(_)      => 0
          case Operand.Memory(_)        => 0
          case Operand.MemoryFromRegister(reg, offset) =>
            getFromMemory(List(registers(accessLookup(reg).index) + offset))
          case Operand.MemoryFromRegisterOffsetFromRegister(reg, offsetReg) =>
            getFromMemory(List(registers(accessLookup(reg).index) + registers(accessLookup(offsetReg).index)))
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
        if (s.startsWith("word [")) {
          val ref = s.slice(6, s.length - 1)
          ref.toIntOption match {
            case Some(initialIndex) =>
              Operand.Memory(List(initialIndex, initialIndex + 1))
            case None =>
              val add        = ref.contains("+")
              val splitArray = (if (add) ref.split("\\+") else ref.split("-")).map(_.trim)
              splitArray match {
                case Array(reg, offset) if add =>
                  offset.toIntOption
                    .map(Operand.MemoryFromRegister(reg, _))
                    .getOrElse(Operand.MemoryFromRegisterOffsetFromRegister(reg, offset))
                case Array(reg, offset) => Operand.MemoryFromRegister(reg, -offset.toInt)
                case arr                => Operand.MemoryFromRegister(arr.head, 0)
              }
          }
        } else if (s.startsWith("[")) {
          val ref = s.slice(1, s.length - 1)
          ref.toIntOption match {
            case Some(initialIndex) =>
              Operand.Memory(List(initialIndex, initialIndex + 1))
            case None =>
              val add        = ref.contains("+")
              val splitArray = (if (add) ref.split("\\+") else ref.split("-")).map(_.trim)
              splitArray match {
                case Array(reg, offset) if add =>
                  offset.toIntOption
                    .map(Operand.MemoryFromRegister(reg, _))
                    .getOrElse(Operand.MemoryFromRegisterOffsetFromRegister(reg, offset))
                case Array(reg, offset) => Operand.MemoryFromRegister(reg, -offset.toInt)
                case arr                => Operand.MemoryFromRegister(arr.head, 0)
              }
          }
        } else {
          s.toIntOption.map(_.toChar) match {
            case Some(value) => Operand.Immediate(value)
            case None        => Operand.Register(s)
          }
        }
      }

      val instruction: Instruction = Instruction.from(current)

      result(i) = InstructionApplication(instruction, operands, size.toChar)

      i += 1
    }

    result
  }
}

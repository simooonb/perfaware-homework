package bar.simon.perfaware.part1

import bar.simon.perfaware.part1.Instruction.Mov

object RegisterSimulation {
  val registers: Array[Int] = Array.fill(8)(0)

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
    val result = Array.ofDim[String](instructionApplications.length + 2 + registers.length)
    var i      = 0

    while (i < instructionApplications.length) {
      val current = instructionApplications(i)

      val destinationRegister = current.operands(0) match {
        case Operand.Immediate(_)  => "" // should not happen
        case Operand.Register(reg) => reg
      }

      val oldValue     = registers(accessLookup(destinationRegister).index)
      val valueToWrite = current.operands(1) match {
        case Operand.Immediate(value) => value
        case Operand.Register(reg)    => registers(accessLookup(reg).index)
      }

      val access = accessLookup(destinationRegister)

      val newValue: Int =
        if (access.low && access.high) {
          valueToWrite
        } else if (access.low) {
          val highValue = (registers(access.index) & 0xff >>> 4) << 4
          val lowValue  = valueToWrite & 0xff & ~(1 << 4)

          lowValue & highValue
        } else if (access.high) {
          val lowValue  = registers(access.index) & 0xff & ~(1 << 4)
          val highValue = (valueToWrite & 0xff >>> 4) << 4

          lowValue & highValue
        } else {
          println("problem")
          0
        }

      val destStr = current.operands(0).str
      result(i) = f"mov $destStr, ${current.operands(1).str} ; $destStr:0x$oldValue%01x->0x$newValue%01x"
      registers(access.index) = newValue

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

    result.mkString("\n")
  }

  def translate(asm: Array[String]): Array[InstructionApplication] = {
    val result = Array.ofDim[InstructionApplication](asm.length)
    var i      = 0

    while (i < result.length) {
      val current = asm(i)

      if (current.startsWith("mov")) {
        val operands: Array[Operand] = current.drop(4).split(", ").map { s =>
          s.toIntOption match {
            case Some(value) => Operand.Immediate(value)
            case None        => Operand.Register(s)
          }
        }

        result(i) = InstructionApplication(Mov, operands)
      }

      i += 1
    }

    result
  }
}

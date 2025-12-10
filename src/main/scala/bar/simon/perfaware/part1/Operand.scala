package bar.simon.perfaware.part1

sealed trait Operand {
  val str: String
}

object Operand {
  case class Immediate(value: Char) extends Operand {
    val str: String = value.toInt.toString
  }

  case class Register(value: String) extends Operand {
    val str: String = value
  }

  case class Memory(indices: List[Int]) extends Operand {
    val str: String = {
      val idx  = indices.head
      val sign = if (idx >= 0) "+" else "-"
      if (indices.size > 1) s"word [$sign$idx]" else s"[$sign$idx]"
    }
  }

  case class MemoryFromRegister(register: String, offset: Int) extends Operand {
    val str: String = s"[$register${if (offset >= 0) "+" else "-"}$offset]"
  }

  case class MemoryFromRegisterOffsetFromRegister(register: String, offsetRegister: String) extends Operand {
    val str: String = s"[$register+$offsetRegister]"
  }
}

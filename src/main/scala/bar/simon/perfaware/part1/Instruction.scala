package bar.simon.perfaware.part1

import bar.simon.perfaware.part1.Instruction._

sealed trait Instruction {
  val opcode: String

  def isNotMov: Boolean = this != Mov
}

object Instruction {
  val all = List(Mov, Add, Sub, Cmp, Jne)

  def from(str: String): Instruction =
    all.find(i => str.startsWith(i.opcode)).getOrElse(Unknown)

  case object Mov extends Instruction {
    override val opcode: String = "mov"
  }

  case object Add extends Instruction {
    override val opcode: String = "add"
  }

  case object Sub extends Instruction {
    override val opcode: String = "sub"
  }

  case object Cmp extends Instruction {
    override val opcode: String = "cmp"
  }

  case object Jne extends Instruction {
    override val opcode: String = "jne"
  }

  case object Unknown extends Instruction {
    override val opcode: String = "?"
  }
}

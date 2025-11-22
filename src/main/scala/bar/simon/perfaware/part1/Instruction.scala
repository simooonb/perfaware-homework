package bar.simon.perfaware.part1

import bar.simon.perfaware.part1.Instruction._

sealed trait Instruction {
  val opcode: String

  def isNotMov: Boolean = this match {
    case Mov => false
    case Sub => true
    case Add => true
    case Cmp => true
  }
}

object Instruction {
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
}

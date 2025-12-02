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
}

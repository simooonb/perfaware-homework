package bar.simon.perfaware.part1

case class Register(
    name: String,
    high: Byte,
    low: Byte
)

case class RegisterAccess(index: Int, high: Boolean, low: Boolean)

case class Flag(name: String, var value: Boolean) {
  def setStr(newValue: Boolean): String =
    if (newValue != value && newValue)
      s"->$name"
    else if (newValue != value)
      s"$name->"
    else
      ""

  def str: String = if (value) name else ""
}

package bar.simon.perfaware.part1

case class Register(
    name: String,
    high: Byte,
    low: Byte
)

case class RegisterAccess(index: Int, high: Boolean, low: Boolean)
